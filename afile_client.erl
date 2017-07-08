-module (afile_client).
-export ([ls/1, get_file/2, rename/3]).

ls(Server) ->
  Server ! {self(), list_dir},
  receive
    {Server, FileList} ->
      FileList
  end.

get_file(Server, File) ->
  Server ! {self(), {get_file,File}},
  receive
    {Server, FileContent} ->
      FileContent
  end.

rename(Server, File, NewFile) ->
  Server ! {self(), {rename,File,NewFile}},
  receive
    {Server, ok} ->
      io:format("filename changed！");
    {Server, {error, Error}} ->
      io:format("filename change failed, reason: ~p~n", [Error])
  end.