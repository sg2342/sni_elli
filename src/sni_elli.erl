-module(sni_elli).

-behaviour(gen_statem).
-behaviour(snierl_con).
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-export([handle/2, handle_event/3]).

-export([proxy/1]).

-export([init/1, callback_mode/0, handle_event/4, start_link/0, terminate/3]).

-ignore_xref({start_link, 0}).


-spec proxy(snierl_con:proxy_map()) -> ok.
proxy(#{ tls := Tls } = M) ->
    {ok, Pid} = sni_elli_sup:start_child(),
    ok = ssl:controlling_process(Tls, Pid),
    gen_statem:cast(Pid, maps:remove(hs_opts, M)).


start_link() -> gen_statem:start_link(?MODULE, [], []).


callback_mode() -> handle_event_function.


init([]) -> {ok, undefined, #{}}.


terminate(_Reason, _State, #{ tls := Tls }) -> ssl:close(Tls);
terminate(_Reason, _State, _Data) -> ok.


handle_event(cast, #{ } = M, undefined, #{}) ->
    keepalive_loop(M),
    {stop, normal, M}.


handle_event(_Event, _Args, _Config) -> ok.


handle(#req{ method = 'GET', raw_path = RPath }, [Dir]) ->
    handle1(local_path(RPath, Dir));
handle(#req{ }, _) -> {403, [], <<"Forbidden">>}.


handle1(false) -> {403, [], <<"Forbidden">>};
handle1(Path) -> handle2(elli_util:file_size(Path), Path).

handle2({error, _}, _Path) -> {404, [], <<"Not Found">>};
handle2(Size, Path) ->
    {ok, [{"Content-Length", Size} | mime_type(Path)], {file, Path}}.


keepalive_loop(#{ tls := Tls, elli_opts := Opts, elli_callback := Callback }) ->
    elli_http:keepalive_loop({ssl, Tls}, Opts, Callback), stop;
keepalive_loop(#{ tls := _, sni := HostName, elli_opts := _ } = M) ->
    {ok, TopDir} = application:get_env(top_dir),
    Dir = filename:join(TopDir, HostName),
    keepalive_loop(M#{ elli_callback => {?MODULE, [ Dir ]} });
keepalive_loop(#{ tls := _ } = M) ->
    {ok, Opts} = application:get_env(elli_opts),
    keepalive_loop(M#{ elli_opts => Opts }).


local_path(<<$/>>, Dir) -> local_path(<<"/index.html">>, Dir);
local_path(<<$/, Path/binary>>, Dir) ->
    F1 = filename:join(Dir, Path),
    F2 = filename:split(F1),
    F3 = lists:filter(fun (<<"..">>) -> false; (_) -> true end, F2),
    F3 == F2 andalso F1;
local_path(_, _) -> false.


mime_type(Path) -> mime_type1(filename:extension(Path)).

mime_type1(<<$., Ext/binary>>) ->
    mime_type2(proplists:get_value(binary_to_list(Ext), mime_types3(),
				   undefined));
mime_type1(_) -> [].

mime_type2(undefined) -> [];
mime_type2(CT) -> [{"Content-Type", CT}].

mime_types3() ->
    [{"ice","x-conference/x-cooltalk"},
     {"movie","video/x-sgi-movie"},
     {"avi","video/x-msvideo"},
     {"mxu","video/vnd.mpegurl"},
     {"qt","video/quicktime"},
     {"mov","video/quicktime"},
     {"mpeg","video/mpeg"},
     {"mpg","video/mpeg"},
     {"mpe","video/mpeg"},
     {"xml","text/xml"},
     {"xsl","text/xml"},
     {"shtml","text/x-server-parsed-html"},
     {"etx","text/x-setext"},
     {"wmls","text/vnd.wap.wmlscript"},
     {"wml","text/vnd.wap.wml"},
     {"tsv","text/tab-separated-values"},
     {"sgml","text/sgml"},
     {"sgm","text/sgml"},
     {"rtf","text/rtf"},
     {"rtx","text/richtext"},
     {"asc","text/plain"},
     {"txt","text/plain"},
     {"html","text/html"},
     {"htm","text/html"},
     {"css","text/css"},
     {"wrl","model/vrml"},
     {"vrml","model/vrml"},
     {"msh","model/mesh"},
     {"mesh","model/mesh"},
     {"silo","model/mesh"},
     {"igs","model/iges"},
     {"iges","model/iges"},
     {"xwd","image/x-xwindowdump"},
     {"xpm","image/x-xpixmap"},
     {"xbm","image/x-xbitmap"},
     {"rgb","image/x-rgb"},
     {"ppm","image/x-portable-pixmap"},
     {"pgm","image/x-portable-graymap"},
     {"pbm","image/x-portable-bitmap"},
     {"pnm","image/x-portable-anymap"},
     {"ras","image/x-cmu-raster"},
     {"wbmp","image/vnd.wap.wbmp"},
     {"tiff","image/tiff"},
     {"tif","image/tiff"},
     {"png","image/png"},
     {"jpeg","image/jpeg"},
     {"jpg","image/jpeg"},
     {"jpe","image/jpeg"},
     {"ief","image/ief"},
     {"gif","image/gif"},
     {"bmp","image/bmp"},
     {"xyz","chemical/x-xyz"},
     {"pdb","chemical/x-pdb"},
     {"wav","audio/x-wav"},
     {"ra","audio/x-realaudio"},
     {"rpm","audio/x-pn-realaudio-plugin"},
     {"ram","audio/x-pn-realaudio"},
     {"rm","audio/x-pn-realaudio"},
     {"m3u","audio/x-mpegurl"},
     {"aif","audio/x-aiff"},
     {"aiff","audio/x-aiff"},
     {"aifc","audio/x-aiff"},
     {"mpga","audio/mpeg"},
     {"mp2","audio/mpeg"},
     {"mp3","audio/mpeg"},
     {"mid","audio/midi"},
     {"midi","audio/midi"},
     {"kar","audio/midi"},
     {"au","audio/basic"},
     {"snd","audio/basic"},
     {"zip","application/zip"},
     {"src","application/x-wais-source"},
     {"ustar","application/x-ustar"},
     {"ms","application/x-troff-ms"},
     {"me","application/x-troff-me"},
     {"man","application/x-troff-man"},
     {"t","application/x-troff"},
     {"tr","application/x-troff"},
     {"roff","application/x-troff"},
     {"texinfo","application/x-texinfo"},
     {"texi","application/x-texinfo"},
     {"tex","application/x-tex"},
     {"tcl","application/x-tcl"},
     {"tar","application/x-tar"},
     {"sv4crc","application/x-sv4crc"},
     {"sv4cpio","application/x-sv4cpio"},
     {"sit","application/x-stuffit"},
     {"swf","application/x-shockwave-flash"},
     {"shar","application/x-shar"},
     {"sh","application/x-sh"},
     {"nc","application/x-netcdf"},
     {"cdf","application/x-netcdf"},
     {"latex","application/x-latex"},
     {"skp","application/x-koan"},
     {"skd","application/x-koan"},
     {"skt","application/x-koan"},
     {"skm","application/x-koan"},
     {"js","application/x-javascript"},
     {"hdf","application/x-hdf"},
     {"gtar","application/x-gtar"},
     {"spl","application/x-futuresplash"},
     {"dvi","application/x-dvi"},
     {"dcr","application/x-director"},
     {"dir","application/x-director"},
     {"dxr","application/x-director"},
     {"csh","application/x-csh"},
     {"cpio","application/x-cpio"},
     {"pgn","application/x-chess-pgn"},
     {"vcd","application/x-cdlink"},
     {"bcpio","application/x-bcpio"},
     {"wmlsc","application/vnd.wap.wmlscriptc"},
     {"wmlc","application/vnd.wap.wmlc"},
     {"wbxml","application/vnd.wap.wbxml"},
     {"ppt","application/vnd.ms-powerpoint"},
     {"xls","application/vnd.ms-excel"},
     {"mif","application/vnd.mif"},
     {"smi","application/smil"},
     {"smil","application/smil"},
     {"ai","application/postscript"},
     {"eps","application/postscript"},
     {"ps","application/postscript"},
     {"pdf","application/pdf"},
     {"oda","application/oda"},
     {"bin","application/octet-stream"},
     {"dms","application/octet-stream"},
     {"lha","application/octet-stream"},
     {"lzh","application/octet-stream"},
     {"exe","application/octet-stream"},
     {"class","application/octet-stream"},
     {"so","application/octet-stream"},
     {"dll","application/octet-stream"},
     {"doc","application/msword"},
     {"cpt","application/mac-compactpro"},
     {"hqx","application/mac-binhex40"},
     {"ez","application/andrew-inset"}].
