#ifndef __ANEMONE_SEGV_H
#define __ANEMONE_SEGV_H

void anemone_segv_remove_handler ();
void anemone_segv_install_handler (const char *user_data, size_t user_data_size);

#endif//__ANEMONE_SEGV_H
