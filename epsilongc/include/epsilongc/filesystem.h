/* This file is part of GNU epsilon.

   Copyright (C) 2005 Luca Saiu
   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

   GNU epsilon is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU epsilon is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>. */


/* A simple high-level filesystem interface. */

#ifndef EPSILONGC_FILESYSTEM_H_
#define EPSILONGC_FILESYSTEM_H_

#include <stdio.h>
#include <time.h>
#include "types.h"
#include "list.h"

/* Return a nonzero value iff the given file exists and is readable: */
int epsilongc_exists_and_is_readable(const char *file_name);

/* Return a nonzero value iff the given file exists, is readable, and isn't
   older than the other given file (which must exist, otherwise we fail with
   a fatal error): */
int epsilongc_exists_and_is_readable_and_is_not_older_than(const char *file_name,
						 const char *other_file_name);

/* Make an empty temporary directory and return an its absolute pathname.
   If destroy_at_exit_time is nonzero then the temporary directory and all
   its content will be automatically destroyed at exit() time; in this case,
   and only in this case, the returned string should *not* be destroyed by
   the user: it will be needed at exit() time for destroying the directory.
   Otherwise it's the user responsibility to free() the string.
   Note that in any case the caller is free to destroy the temporary
   directory before if he/she wishes. If a temporary directory can't be
   created then NULL is returned: */
char* epsilongc_create_temporary_directory_within(const char *parent_directory,
                                            const int destroy_at_exit_time);
char* epsilongc_create_temporary_directory(const int destroy_at_exit_time);

/* Destroy the given directory and all the files and directory it
   contains: */
void epsilongc_remove_directory_and_all_its_content(const char *directory);

/* A search path is simply a list of strings, holding the names of the
   directories to be looked at, in order. If the current directory should
   also be looked at then this information must be *explicitly* included 
   in the search path, as a "." element. */

/* Parse a search path expressed as a string, with the usual UNIX syntax,
   and translate it into a new list of malloc()ed strings: */
struct epsilongc_list* epsilongc_parse_search_path(const char *search_path);

/* Destroy the given search path, including both the list structure and the
   individual strings, which must be malloc()ed buffers with no sharing: */
void epsilongc_destroy_search_path(struct epsilongc_list *search_path);

/* Return the first matching readable occurrence of the given filename as a
   new malloc()ed string, or NULL if no matching file exists. If the file
   name is absolute then return a copy of it, ignoring the search path: */
char* epsilongc_locate_in_search_path(const char *file_name,
			    const struct epsilongc_list *search_path);

/* Simply return the result of fopen() on the first matching file in the search
   path. If no matching file exists then NULL is returned. If the file name is
   absolute then return a copy of it, ignoring the search path: */
FILE* epsilongc_fopen_in_search_path(const char *relative_file_name,
			   const char *open_type,
			   const struct epsilongc_list *search_path);

/* Return a new malloc()ed string holding the name of the current working
   directory: */
char* epsilongc_get_working_directory(void);

/* Return the last modification time of the given file; fail with a fatal
   error if the file does not exist or its attributes are not readable. 
   Symbolic links are automatically followed. */
time_t epsilongc_file_name_to_last_modification_time(const char *file_name);

/* By convention an extension does *not* include the leading dot. Note that
   the following functions don't check the *existence* of any file; they just
   work on their names. */

/* Given a file name return its extension as a new malloc()ed string, or 
   NULL if it has no extension: */
char* epsilongc_file_name_to_extension(const char *file_name);

/* *Non-destructively* replace the given filename extension, i.e. return a
   new malloc()ed string with the given extension instead of the current one.
   If the given filename has no extension then the new extension is appended: */
char* epsilongc_replace_extension(const char *file_name, const char *new_extension);

/* Return nonzero iff the given pathname is relative: */
int epsilongc_is_path_name_relative(const char *path_name);

/* Return nonzero iff the given pathname is absolute: */
int epsilongc_is_path_name_absolute(const char *path_name);

/* Given a pathname return a new malloc()ed string with only the file or
   directory part. This correctly supports ".", "..", "~" and "/". If a
   pathname has '/' as its last character then a (new) empty string is
   returned as the file name: */
char* epsilongc_path_name_to_file_part(const char *path_name);
char* epsilongc_path_name_to_directory_part(const char *path_name);

/* Return the length of the given file, or -1 on failure: */
long epsilongc_file_length(const char *path_name);

/* Return a new malloc()ed string filled with the whole content of the given
   file, or NULL in case of errors. Of course this shouldn't be used with
   very large files whose content doesn't fit in core. */
char* epsilongc_get_the_full_content_of(const char *path_name);

//#warning To do: decide a coherent policy about the symbol table files

//#warning from a pathname.

//#warning To do: a C function to rename an .epsilongcl file into the canonical name it
//#warning should have, i.e. MANGLED_NAME.epsilongcl

#define EPSILONGCL_EXTENSION "epsilongcl"
#define SYMBOL_TABLE_EXTENSION "epsilongcs"

#endif // #ifndef EPSILONGC_FILESYSTEM_H_
