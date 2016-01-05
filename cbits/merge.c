#include <fcntl.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <unistd.h>

#include "base.h"
#include "memcmp8.h"
#include "memcmp16.h"

#define MERGE_BUFFER_SIZE  16384

typedef struct input_s
{
    int         fd;

    /* add some extra bytes at the end for memcmp16 */
    char        buf[MERGE_BUFFER_SIZE+16];

    /* 0 <= buf_end <= MERGE_BUFFER_SIZE */
    /* buf_end == 0 ==> uninitialised || finished */
    uint64_t    buf_end;

    uint64_t    bytes_left;

    /* must be start of line */
    /* buf <= entity_start <= buf + MERGE_BUFFER_size */
    /* entity_start == buf || *(entity_start-1) == '\n' */
    const char*       entity_start;
    /* *entity_end == '|' */
    const char*       entity_end;

    /* *(date_start-1) == '|' */
    const char*       date_start;
    /* *date_end == '\n' */
    const char*       date_end;
} input_t;


typedef char* error_t;
static error_t allocate_error(const char* err)
{
    uint64_t len = strlen(err);
    char* into = malloc(len+1);
    memcpy(into, err, len+1);

    return into;
}
static const error_t no_error = 0;


static INLINE error_t read_input_line(input_t* input);

static INLINE const char* find_date_start(const char* start, const char* end, uint64_t format)
{
    if (end - start >= format   &&
        end[-format] == '|') {
        return end - format + 1;
    }
    return 0;
}

/* input->buf <= start <= newline <= input->buf + MERGE_BUFFER_SIZE */
/* !input.finished */
static INLINE error_t parse_input_line(input_t* input, const char* start, const char* newline)
{
    const char*    buf  = input->buf;
    uint64_t buf_end    = input->buf_end;
    uint64_t bytes_left = input->bytes_left;

    if (buf_end == 0) {
        return no_error;
    }


    if (!newline) {
        /* if it's the last line, just output the rest */
        if (bytes_left == 0) {
            /* make sure that the actual end of the buffer is treated as the newline */
            newline = buf + buf_end;
        } else
            return allocate_error("parse_input_line: no newline found: either no newline at end of file, or line is too large to fit in memory");
    }


    /* Find the first pipe after the start of line */
    const char* entity_end    = memchr(start, '|', (buf + buf_end) - start);
    if (!entity_end) {
        return allocate_error("parse_input_line: no field separator in input line");
    }

    input->entity_start = start;
    input->entity_end   = entity_end;
    input->date_end     = newline;

    const char* date_start = find_date_start(start, newline, sizeof("YYYY-MM-DD"));
    if (!date_start) {
        date_start   = find_date_start(start, newline, sizeof("YYYY-MM-DDThh:mm:ssZ"));
    }
    if (date_start) {
        input->date_start = date_start;
        return no_error;
    } else {
        return allocate_error("parse_input_line: unknown date format; should be YYYY-MM-DD or YYYY-MM-DDThh:mm:ssZ");
    }
}

/* !input.finished */
static INLINE error_t read_input_line(input_t* input)
{
    int      fd         = input->fd;
    char*    buf        = input->buf;
    uint64_t buf_end    = input->buf_end;
    uint64_t bytes_left = input->bytes_left;

    /* deal with the case where there was no last newline, because we can't read any more bytes */
    if (input->date_end == buf + buf_end && bytes_left == 0) {
        input->buf_end = 0;
        return no_error;
    } if (buf_end == 0)
    {
        /* input.uninitialised */

        /* read no more than bytes_left */
        uint64_t num_to_read = (MERGE_BUFFER_SIZE < bytes_left) ? MERGE_BUFFER_SIZE : bytes_left;
        buf_end  = read(fd, buf, num_to_read);
        if (buf_end == -1) {
            return allocate_error("read_input_line: cannot read from input file");
        }
        bytes_left -= buf_end;

        const char* newline   = memchr(buf, '\n', buf_end);

        input->buf_end      = buf_end;
        input->bytes_left   = bytes_left;
        input->entity_start = buf;
        input->entity_end   = buf;
        input->date_start   = buf;
        input->date_end     = buf;

        return parse_input_line(input, buf, newline);
    }
    else
    {
        /* !input.finished && !.input.uninitialised */

        /* *input->date_end == '\n' */
        const char* start     = input->date_end + 1;
        const char* end       = buf + buf_end;
        uint64_t remain = end - start;

        const char* newline   = memchr(start, '\n', remain);
        /* newline == NULL || buf <= newline <= end < buf + MERGE_BUFFER_SIZE */

        /* newline == NULL ==> all [start..end) /= '\n' */
        /* newline /= NULL ==> all [start..newline) /= '\n' */
        if (!newline) {
            /* all [start..end) /= '\n' */
            /* all [start..start + remain) /= '\n' */
            memmove(buf, start, remain);
            /* all [input->buf..input->buf + remain) /= '\n' */

            uint64_t num_to_read = MERGE_BUFFER_SIZE - remain;
            /* read no more than bytes_left */
            num_to_read = (num_to_read < bytes_left) ? num_to_read : bytes_left;
            char* read_into = buf + remain;
            uint64_t bytes_read = read(fd, read_into, num_to_read);
            if (bytes_read == -1) {
                return allocate_error("read_input_line: cannot read from input file");
            }

            input->bytes_left = bytes_left - bytes_read;
            input->buf_end = remain + bytes_read;
            newline     = memchr(buf, '\n', input->buf_end);
            start       = buf;
            /* all [start..newline) /= '\n' */
        }
        return parse_input_line(input, start, newline);
    }
}


typedef struct output_s
{
    int         fd;

    char        buf[MERGE_BUFFER_SIZE];
    uint64_t    buf_end;
} output_t;

static INLINE error_t write_output_line(output_t* out, const char* start, const char* end)
{
    int      fd         = out->fd;
    char*    buf        = out->buf;
    uint64_t buf_end    = out->buf_end;

    /* don't include newline in [start,end); it might not be there */
    /* instead add one on regardless so we have one at the end, no matter what */
    uint64_t len        = end - start + 1;
    uint64_t left       = buf_end + len;

    if (left < MERGE_BUFFER_SIZE) {
        memcpy(buf + buf_end, start, len);
        out->buf_end = left;
        buf[left-1] = '\n';
    } else {
        int64_t written = write(fd, buf, buf_end);

        if (written < buf_end) {
            return allocate_error("write_output_line: cannot write to file");
        }

        memcpy(buf, start, len);
        out->buf_end = len;
        buf[len] = '\n';
    }

    return no_error;
}

static error_t flush_output(output_t* out)
{
    int      fd         = out->fd;
    const char*    buf  = out->buf;
    uint64_t buf_end    = out->buf_end;

    int64_t written = write(fd, buf, buf_end);
    if (written < buf_end) {
        return allocate_error("flush_output: cannot write to file");
    }

    out->buf_end = 0;
    return no_error;
}

static INLINE error_t write_from_input(input_t* input, output_t* out)
{
    error_t err;

    err = write_output_line(out, input->entity_start, input->date_end);
    if (err) return err;

    return read_input_line(input);
}

static INLINE error_t bulk_write_leftovers(input_t* input, output_t* out)
{
    /* flush any remaining output because we will no longer be using the output buffer */
    flush_output(out);

    int      in_fd      = input->fd;
    int      out_fd     = out->fd;

    /* use the input buffer because it may already have stuff in it */
    char*    buf        = input->buf;
    uint64_t buf_end    = input->buf_end;

    /* spit out the remnants from the input buffer */
    const char* entity_start = input->entity_start;
    if (entity_start != buf + buf_end) {
        uint64_t len = buf + buf_end - entity_start;

        int64_t written = write(out_fd, entity_start, len);
        if (written < len) {
            return allocate_error("bulk_write_leftovers: cannot write to file");
        }
    }

    uint64_t bytes_left = input->bytes_left;

    /* do the rest by reading and writing */
    while (1) {
        /* read into input buffer. if bytes_left == 0, we will just return with bytes_read == 0 */
        uint64_t num_to_read = MIN(MERGE_BUFFER_SIZE, bytes_left);
        int64_t bytes_read = read(in_fd, buf, num_to_read);
        bytes_left -= bytes_read;

        /* check if we are finished or had an error */
        if (bytes_read == 0) {
            return no_error;
        } else if (bytes_read < 0) {
            return allocate_error("bulk_write_leftovers: cannot read from input");
        }

        /* write whatever we managed to read */
        int64_t written = write(out_fd, buf, bytes_read);
        if (written < bytes_read) {
            return allocate_error("bulk_write_leftovers: cannot write to file");
        }
    }
}


typedef struct merge_s
{
    output_t    output;

    uint64_t    num_inputs;
    input_t*    inputs;
} merge_t;

static error_t merge_init(merge_t* merge)
{
    uint64_t num_inputs = merge->num_inputs;
    input_t* inputs     = merge->inputs;

    error_t err;
    for (uint64_t i = 0; i != num_inputs; ++i) {
        err = read_input_line(&inputs[i]);
        if (err) return err;
    }

    return no_error;
}

static INLINE int64_t compare2(const char* buf1, const char* end1, const char* buf2, const char* end2)
{
    uint64_t len1 = end1 - buf1;
    uint64_t len2 = end2 - buf2;

    uint64_t min = (len1 < len2) ? len1 : len2;
    int64_t cmp = memcmp8(buf1, buf2, min);
    uint64_t len_diff = len1 - len2;
    if (cmp == 0)
        /* if buf1 is shorter, it is smaller */
        return len1 - len2;
    else
        return cmp;
}

static INLINE void find_smallest_line(merge_t* merge, uint64_t* out_smallest, uint64_t* out_alive)
{
    uint64_t num_inputs = merge->num_inputs;
    input_t* inputs     = merge->inputs;

    uint64_t smallest = 0;
    uint64_t alive = 0;

    for (uint64_t i = 0; i != num_inputs; ++i) {
        if (inputs[i].buf_end != 0) {
            alive++;

            if (alive == 1) {
                smallest = i;
            } else {
                /* compare smallest with this one */
                int64_t cmp_ent = compare2( inputs[i].entity_start, inputs[i].entity_end
                                      , inputs[smallest].entity_start, inputs[smallest].entity_end);

                if (cmp_ent == 0) {
                    int64_t cmp_dat = compare2( inputs[i].date_start, inputs[i].date_end
                                          , inputs[smallest].date_start, inputs[smallest].date_end);


                    if (cmp_dat < 0) smallest = i;
                }
                else if (cmp_ent < 0) {
                    smallest = i;
                }
            }
        }
    }

    *out_smallest = smallest;
    *out_alive    = alive;
}

static INLINE error_t merge_lines(merge_t* merge)
{
    output_t* output    = &merge->output;
    input_t* inputs     = merge->inputs;

    uint64_t smallest = 0;
    uint64_t alive = 0;

    error_t err;

    err = merge_init(merge);
    if (err) return err;

    find_smallest_line(merge, &smallest, &alive);

    while (alive > 1) {
        err = write_from_input(&inputs[smallest], output);
        if (err) return err;

        find_smallest_line(merge, &smallest, &alive);
    }
    if (alive == 1) {
        return bulk_write_leftovers(&inputs[smallest], output);
    } else {
        return flush_output(output);
    }
}

