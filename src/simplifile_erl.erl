%%% --------------------------------------------------
%%% @author Benjamin Peinhardt <benjaminpeinhardt@gmail.com>
%%% @doc Erlang ffi glue code for the simplifile Gleam package.
%%% @end
%%% --------------------------------------------------

-module(simplifile_erl).

%% API
-export([read_file/1, append_file/2, write_file/2, delete_file/1, delete_directory/1,
         recursive_delete/1, list_directory/1, make_directory/1, make_symlink/2,
         is_file/1, create_dir_all/1, rename_file/2, set_permissions/2,
         is_valid_directory/1, is_valid_file/1, is_valid_symlink/1, file_info/1]).

-include_lib("kernel/include/file.hrl").

%% A macro for checking whether the error returned is one of the atoms for a posixe error.
-define(is_posix_error(Error),
        Error =:= eacces
        orelse Error =:= eagain
        orelse Error =:= ebadf
        orelse Error =:= ebadmsg
        orelse Error =:= ebusy
        orelse Error =:= edeadlk
        orelse Error =:= edeadlock
        orelse Error =:= edquot
        orelse Error =:= eexist
        orelse Error =:= efault
        orelse Error =:= efbig
        orelse Error =:= eftype
        orelse Error =:= eintr
        orelse Error =:= einval
        orelse Error =:= eio
        orelse Error =:= eisdir
        orelse Error =:= eloop
        orelse Error =:= emfile
        orelse Error =:= emlink
        orelse Error =:= emultihop
        orelse Error =:= enametoolong
        orelse Error =:= enfile
        orelse Error =:= enobufs
        orelse Error =:= enodev
        orelse Error =:= enolck
        orelse Error =:= enolink
        orelse Error =:= enoent
        orelse Error =:= enomem
        orelse Error =:= enospc
        orelse Error =:= enosr
        orelse Error =:= enostr
        orelse Error =:= enosys
        orelse Error =:= enotblk
        orelse Error =:= enotdir
        orelse Error =:= enotsup
        orelse Error =:= enxio
        orelse Error =:= eopnotsupp
        orelse Error =:= eoverflow
        orelse Error =:= eperm
        orelse Error =:= epipe
        orelse Error =:= erange
        orelse Error =:= erofs
        orelse Error =:= espipe
        orelse Error =:= esrch
        orelse Error =:= estale
        orelse Error =:= etxtbsy
        orelse Error =:= exdev).

%% Only return the error if it's a posix error. Also converts ok to {ok, nil},
%% as gleam doesn't have atoms.
posix_result(Result) ->
    case Result of
        ok ->
            {ok, nil};
        {ok, Value} ->
            {ok, Value};
        {error, Reason} when ?is_posix_error(Reason) ->
            {error, Reason}
    end.

%% Read the binary contents of a file
read_file(Filename) ->
    posix_result(file:read_file(Filename)).

%% Write bytes to a file
write_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents)).

%% Append bytes to a file
append_file(Contents, Filename) ->
    posix_result(file:write_file(Filename, Contents, [append])).

%% Delete the file at the given path
delete_file(Filename) ->
    posix_result(file:delete(Filename)).

%% Create a directory at the given path. Missing parent directories are not created.
make_directory(Dir) ->
    posix_result(file:make_dir(Dir)).

%% Create a symbolic link New to the file or directory Existing (does not need to exist).
make_symlink(Existing, New) ->
    posix_result(file:make_symlink(Existing, New)).

%% List the contents of a directory
list_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Filenames} ->
            {ok, [unicode:characters_to_binary(Filename) || Filename <- Filenames]};
        {error, Reason} when ?is_posix_error(Reason) ->
            {error, Reason}
    end.

%% Delete a directory at the given path. Directory must be empty.
delete_directory(Dir) ->
    posix_result(file:del_dir(Dir)).

%% Deletes a file/directory and everything in it.
recursive_delete(Dir) ->
    posix_result(file:del_dir_r(Dir)).

%% Checks whether a given file exists and is a file (as opposed to a directory)
is_file(Filename) ->
    not (file:read_file_info(Filename) == {error, enoent}) and not filelib:is_dir(Filename).

%% Creates the entire path for a given directory to exist
create_dir_all(Filename) ->
    posix_result(filelib:ensure_dir(Filename)).

%% Move file from one path to another
rename_file(Source, Destination) ->
    posix_result(file:rename(Source, Destination)).

%% Set the permissions for the given file.
set_permissions(Filename, Permissions) ->
    posix_result(file:change_mode(Filename, Permissions)).

is_valid_directory(Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                directory ->
                    {ok, true};
                _ ->
                    {ok, false}
            end;
        {error, enoent} ->
            {ok, false};
        {error, Reason} ->
            posix_result({error, Reason})
    end.

is_valid_file(Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                regular ->
                    {ok, true};
                _ ->
                    {ok, false}
            end;
        {error, enoent} ->
            {ok, false};
        {error, Reason} ->
            posix_result({error, Reason})
    end.

is_valid_symlink(Path) ->
    case file:read_link_info(Path) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                symlink ->
                    {ok, true};
                _ ->
                    {ok, false}
            end;
        {error, enoent} ->
            {ok, false};
        {error, Reason} ->
            posix_result({error, Reason})
    end.

file_info_result(Result) ->
    case Result of
        {ok,
         {file_info,
          Size,
          _Type,
          _Access,
          Atime,
          Mtime,
          Ctime,
          Mode,
          Links,
          MajorDevice,
          _MinorDevice,
          Inode,
          Uid,
          Gid}} ->
            {ok,
             {file_info,
              Size,
              Mode,
              Links,
              Inode,
              Uid,
              Gid,
              MajorDevice,
              Atime,
              Mtime,
              Ctime}};
        {error, Reason} when ?is_posix_error(Reason) ->
            Result
    end.

file_info(Filename) ->
    file_info_result(file:read_file_info(Filename, [{time, posix}])).

