// We want lseek to work with 64-bit offsets
#define _FILE_OFFSET_BITS 64
#include "merge.c"


static void set_blocking_mode (int fd)
{
    const int flags = fcntl (fd, F_GETFL, 0);
    fcntl (fd, F_SETFL, flags & ~O_NONBLOCK);
}


/* this function takes control of, and closes, all filehandles */
/* it returns a newly-allocated error string, or NULL on success. the error must be freed */
/* does not free the argument arrays */
error_t offset_merge
  ( int output_fd
  , uint64_t num_inputs
  , int* input_fds, uint64_t* input_offsets, uint64_t* input_lengths)
{
    merge_t merge;

    set_blocking_mode(output_fd);
    merge.output.fd = output_fd;
    merge.output.buf_end = 0;

    merge.num_inputs     = num_inputs;
    merge.inputs         = calloc(sizeof(input_t), num_inputs);

    for (uint64_t i = 0; i != num_inputs; ++i) {
        int fd = input_fds[i];
        set_blocking_mode(fd);
        off_t ret = lseek(fd, input_offsets[i], SEEK_SET);
        if (ret == -1) {
            return allocate_error("offset_merge: cannot seek in file");
        }

        merge.inputs[i].fd         = fd;
        merge.inputs[i].bytes_left = input_lengths[i];
    }

    error_t err = merge_lines(&merge);

    free(merge.inputs);


    close(output_fd);
    for (uint64_t i = 0; i != num_inputs; ++i) {
        close(input_fds[i]);
    }

    return err;
}
