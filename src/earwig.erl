-module(earwig).
-mode(compile).
-define(OPTS, [
     % {builtins, true} ,{recurse, true},
     {verbose, true}, {warnings, true}
]).
-export([
    demo_vertices/1,
    demo_vertices_edges/1,
    demo_vertices_variable/1
]).
-export([
    % initialize_xref/1,
    % module_vertices/1
]).
% Escript
-export([
    main/1
]).
-export([
    analyze_ebin_dir/1,
    analyze_ebin_dir_application/2,
    all_xref_queries/1
]).

demo_vertices(Module) ->
    {ok, Pid} = xref:start(x, ?OPTS),
    {ok, Module} = xref_add_module(Module),
    {ok, Vertices} = xref_module_vertices(Module),
    io:format("------------------------------------------------~n"),
    io:format("--- Module Vertices ----------------------------~n"),
    io:format("------------------------------------------------~n"),
    ok = lists:foreach(fun({M, F, Arity}) ->
        io:format("--- ~p:~p/~p~n", [M, F, Arity])
    end, Vertices),
    io:format("------------------------------------------------~n"),
    stopped = xref:stop(x),
    Vertices.

demo_vertices_edges(Module) ->
    {ok, Pid} = xref:start(x, ?OPTS),
    {ok, Module} = xref_add_module(Module),
    {ok, VerticesEdges} = xref_module_vertices_edges(Module),
    io:format("------------------------------------------------~n"),
    io:format("--- Module Vertices Edges ----------------------~n"),
    io:format("------------------------------------------------~n"),
    % Ma = Module A, Mb = Module B. Ma CALLS Mb
    ok = lists:foreach(fun({{Ma,Fa,Aa}, {Mb,Fb,Ab}}) ->
        io:format("--- ~p:~p/~p -> ~p:~p/~p~n", [Ma, Fa, Aa, Mb, Fb, Ab])
    end, VerticesEdges),
    io:format("------------------------------------------------~n"),
    stopped = xref:stop(x),
    VerticesEdges.

demo_vertices_variable(Module) ->
    {ok, Pid} = xref:start(x, ?OPTS),
    {ok, Module} = xref_add_module(Module),
    {ok, Vertices} = set_xref_module_vertices_variable(Module),
    io:format("------------------------------------------------~n"),
    io:format("--- Module Vertices ----------------------------~n"),
    io:format("------------------------------------------------~n"),
    ok = lists:foreach(fun({M, F, Arity}) ->
        io:format("--- ~p:~p/~p~n", [M, F, Arity])
    end, Vertices),
    io:format("------------------------------------------------~n"),
    io:format("XREF VARIABLE : ~p~n", [get_xref_module_variable(Module)]),
    stopped = xref:stop(x),
    Vertices.

-spec xref_add_module(Mod :: atom()) -> {ok, Mod :: atom()}.
xref_add_module(Module) ->
    {module, Module} = c:l(Module),
    {file, BeamLocation} = code:is_loaded(Module),
    {ok,Module} = xref:add_module(x, BeamLocation).

xref_module_vertices(_Module) ->
    xref:q(x, "V").

xref_module_vertices_edges(_Module) ->
    xref:q(x, "E | V").

set_xref_module_vertices_variable(Module) ->
    xref:q(x, lists:flatten(io_lib:format("VAR_~p_vertices := V", [Module]))).

get_xref_module_variable(Module) ->
    xref:q(x, lists:flatten(io_lib:format("VAR_~p_vertices", [Module]))).

analyze_ebin_dir(EbinDir) ->
    ok = initialize_xref(x),
    ok = add_ebin(EbinDir),
    io:format("xref query : \"V\"~n~p~n", [xref:q(x, "V")]).

analyze_ebin_dir_application(EbinDir, Application) ->
    G = digraph:new(),
    ok = initialize_xref(XrefRef=Application),
    {ok, Application} = 
        xref:add_application(
            XrefRef,
            EbinDir,
            [{builtins, true}, {name, Application}, {verbose, true}, {warnings, true}]
        ),
    %% Get all the unresolved calls:
    {ok, _UCs} = x_unresolved_calls(Application),
    {ok, Vertices} = x_vertices(Application),
    ok = lists:foreach(fun(Vertex) -> 
        digraph:add_vertex(G, Vertex)
    end, Vertices),
    {ok, Edged} = x_edges(Application),
    ok = lists:foreach(fun({FromVertex, ToVertex}) -> 
        digraph:add_edge(G, FromVertex, ToVertex)
    end, Edged),
    G.

x_unresolved_calls(Ref) ->
    xref:q(Ref, "UC").

x_edges(Ref) ->
    xref:q(Ref, "E").

x_vertices(Ref) ->
    xref:q(Ref, "V").

%% i can crawl a application, and try and determine, what the dependancies should be..
%% 1) by finding calls to a module, i can track in what application that module is ..


% strip new lines
    % parse string with tokens as ":", "(", ")", "->"
    % start recording functions, and don't record duplicates, ( get UNIQUE code calls )

% ) if it's something like: ?MODULE:func ( just record { FILENAME, Line number, MODULE, Function }
% ) same for SomeVar:func()

%% TODO: use a erlang src/ folder, to know what you need to add to your .app.src as dependancies.

% start() ->
%     % observer:start(),
%     % start_file_reader("/Users/rp/hd2/code/pasture/apps/pasture/src").
%     ok.

% analyze_dirs(Dirs) ->

    %% 

    % filelib:fold_files("/fs01/home/a1713/payport/", ".app.src?", true, fun(Filename, Acc) -> 
    %     [Filename|Acc] 
    % end, [])
    
%% ----------------------------------------------------------------------------------------

all_xref_queries(Module) ->
    ok = initialize_xref(x),
    c:l(Module),
    {file, BeamLocation} = code:is_loaded(Module),
    {ok,Module} = xref:add_module(x, BeamLocation),
    io:format("xref query : \"E\"~n~p~n", [xref:q(x, "E")]),
    io:format("xref query : \"V\"~n~p~n", [xref:q(x, "V")]),
    io:format("xref query : \"M\"~n~p~n", [xref:q(x, "M")]),
    io:format("xref query : \"A\"~n~p~n", [xref:q(x, "A")]),
    io:format("xref query : \"R\"~n~p~n", [xref:q(x, "R")]),
    io:format("xref query : \"ME\"~n~p~n", [xref:q(x, "ME")]),
    io:format("xref query : \"AE\"~n~p~n", [xref:q(x, "AE")]),
    io:format("xref query : \"RE\"~n~p~n", [xref:q(x, "RE")]),
    io:format("xref query : \"L\"~n~p~n", [xref:q(x, "L")]),
    io:format("xref query : \"X\"~n~p~n", [xref:q(x, "X")]),
    io:format("xref query : \"F\"~n~p~n", [xref:q(x, "F")]),
    io:format("xref query : \"B\"~n~p~n", [xref:q(x, "B")]),
    io:format("xref query : \"U\"~n~p~n", [xref:q(x, "U")]),
    io:format("xref query : \"UU\"~n~p~n", [xref:q(x, "UU")]),
    io:format("xref query : \"XU\"~n~p~n", [xref:q(x, "XU")]),
    io:format("xref query : \"LU\"~n~p~n", [xref:q(x, "LU")]),
    io:format("xref query : \"LC\"~n~p~n", [xref:q(x, "LC")]),
    io:format("xref query : \"XC\"~n~p~n", [xref:q(x, "XC")]),
    io:format("xref query : \"AM\"~n~p~n", [xref:q(x, "AM")]),
    io:format("xref query : \"UM\"~n~p~n", [xref:q(x, "UM")]),
    io:format("xref query : \"LM\"~n~p~n", [xref:q(x, "LM")]),
    io:format("xref query : \"UC\"~n~p~n", [xref:q(x, "UC")]),
    io:format("xref query : \"EE\"~n~p~n", [xref:q(x, "EE")]),
    io:format("xref query : \"DF\"~n~p~n", [xref:q(x, "DF")]),
    io:format("xref query : \"DF_1\"~n~p~n", [xref:q(x, "DF_1")]),
    io:format("xref query : \"DF_2\"~n~p~n", [xref:q(x, "DF_2")]),
    io:format("xref query : \"DF_3\"~n~p~n", [xref:q(x, "DF_3")]),
    xref:stop(x).



%% ----------------------------------------------------------------------------------------

% start_file_reader(Dir) ->
%     Files = files(Dir),
%     spawn_link( fun() -> report_server(Files) end ).

% report_server(Files) ->
%     tbl = ets:new(tbl, [public, named_table, ordered_set, {write_concurrency, true}]),
%     ok = lists:foreach(fun(Filename) -> spawn_link(fun() -> reader(Filename, self()) end) ! start end, Files),
%     report_server_loop(Files).

% report_server_loop([]) ->
%     io:format("Report Server - All files are done.~n"),
%     receive
%         _Any ->
%             report_server([])
%     end;
% report_server_loop(Files) ->
%     receive
%         {file_done, Filename} ->
%             io:format("file reader done (~p) ...~n", [Filename]),
%             report_server_loop(lists:delete(Filename, Files))
%     end.

% reader(Filename, ReportPid) ->
%     reader(Filename, ReportPid, file:open(Filename, [read, raw, binary, read_ahead])).

% reader(Filename, RP, {error, Reason}) ->
%     RP ! {file_done, Filename},
%     error_logger:error_msg("file ~p ~nreader died : ~p~n", [Filename, {error, Reason}]);
% reader(Filename, RP, {ok, FPID}) ->
%     receive
%         start ->
%             do_file_read(FPID),
%             erlang:process_info(RP),
%             RP ! {file_done, Filename}
%     end.

% do_file_read(FPID) ->
%     do_file_read_loop(FPID, file_read(FPID)).

% do_file_read_loop(FPID, eof) ->
%     ok = file:close(FPID);
% do_file_read_loop(FPID, {ok, Data}) ->
%     parse_data(Data), %% TODO: maybe check duplicates here too...
%     do_file_read_loop(FPID, file_read(FPID)).

% parse_data(Data) ->
%     Lines = binary:split(Data, [<<"\n">>],[global, trim_all]),
%     % maybe use erl_evl scan strings, or something smarter.
%     ok = lists:foreach(fun(Line) ->
%         true = ets:insert(tbl, {erlang:phash2(Line), Line})
%     end, Lines).

% file_read(FPID) ->
%     file:read(FPID, 4096).

%% ---------------------------------------------------------------------------------

% files(Dir) ->
%     filelib:fold_files(Dir, ".[e|h]rl$", true, fun(Filename, Acc) -> [Filename|Acc] end, []).

add_ebin(EbinPath) ->
    try
        case xref:add_directory(x, EbinPath, ?OPTS) of
            {ok,_Modules} ->
                ok;
            {error, xref_base, Reason} ->
                io:format("~p got ~p",[?MODULE, {error, xref_base, Reason}]),
                {error, xref_base, Reason}
        end
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            io:format("~p got ~p, ~p\n~p\n",[?MODULE, C, E, ST]),
            {C,E,ST}
    end.

% remove_ebin() ->
%     ok.

main(["inject",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    spike:connect_and_do(Node, Cookie, inject, [earwig]);
main(["purge",NodeStr,CookieStr]) ->
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    spike:connect_and_do(Node, Cookie, purge, [earwig]);
main(_) ->
    usage().

usage() ->
    io:format("./earwig_injector [inject/purge] NODE COOKIE\n", []),
    timer:sleep(50).

initialize_xref(Ref) ->
    case xref:start(Ref, ?OPTS) of
        {error, {already_started, _}} ->
            ok = xref:stop(Ref),
            {ok, _} = xref:start(Ref),
            ok;
        {ok, _Pid} ->
            ok
    end,
    ok = xref:set_default(Ref, ?OPTS).