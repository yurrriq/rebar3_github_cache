-module(rebar_github_resource).

-behaviour(rebar_resource).

-export([lock/2,download/3,needs_update/2,make_vsn/1]).

-define(DEBUG(Str, Args), rebar_api:debug(Str, Args)).
-define(INFO(Str, Args), rebar_api:info(Str, Args)).
-define(WARN(Str, Args), rebar_api:warn(Str, Args)).

-define(GH(Repo), "https://github.com/" ++ Repo).

-type github_resource() :: {github,Repo::string(),{tag,Vsn::string()}}
                         | {github,Repo,{branch,Branch::string()}}
                         | {github,Repo,{ref,Ref::string()}}
                         | {github,Repo}.

-export_type([github_resource/0]).


-spec lock(AppDir::file:filename_all(), github_resource()) -> Lock when
    Lock :: {github,Repo::string(),{tag,Vsn::string()}}
          | {git,Url::string(),{ref,Ref::string()}}.
lock(_AppDir, {github,Repo,{tag,Vsn}}) ->
  {github,filename:rootname(Repo, ".git"), {tag,Vsn}};
lock(AppDir, {github,Repo,Other}) ->
  rebar_git_resource:lock(AppDir, {git,?GH(Repo),Other});
lock(AppDir, {github,Repo}) ->
  rebar_git_resource:lock(AppDir, {git,?GH(Repo)}).

-spec needs_update(Dir::file:filename_all(), github_resource()) -> boolean().
needs_update(Dir, {github,_Repo,{tag,Vsn}}) ->
  [AppInfo] = rebar_app_discover:find_apps([Dir], all),
  rebar_app_info:original_vsn(AppInfo) =/= ec_cnv:to_list(Vsn);
needs_update(Dir, {github,Repo,Other}) ->
  rebar_git_resource:needs_update(Dir, {git,?GH(Repo),Other});
needs_update(Dir, {github,Repo}) ->
  rebar_git_resource:needs_update(Dir, {git,?GH(Repo)}).

-spec download(Dir::file:filename_all(), github_resource(), rebar_state:t())
              -> {ok,true}
                   | {fetch_fail,Repo::string(),Vsn::string()}
                   | {failed_extract,TarPath::file:filename_all()}
                   | {bad_download,TarPath::file:filename_all()}.
download(Dir, {github,Repo0,{tag,Vsn}}=Dep, State) ->
  Repo1 = filename:rootname(Repo0, ".git"),
  GlobalCacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
  [User,Name] = filename:split(Repo1),
  %% TODO: make this configurable
  CacheDir = filename:join([GlobalCacheDir,"github",User]),
  Archive = Vsn ++ ".tar.gz",
  CachePath = filename:join(CacheDir, Name ++ "-" ++ Vsn),
  ArchivePath = filename:join(["archive",Archive]),
  case rebar_utils:url_append_path(?GH(Repo1), ArchivePath) of
    {ok,Url} ->
      cached_download(Dir, CachePath, Dep, Url, etag(CachePath), State);
    _ ->
      {fetch_fail,Repo1,Vsn}
  end;
download(Dir, {github,Repo,Other}, State) ->
  rebar_git_resource:download(Dir, {git,?GH(Repo),Other}, State);
download(Dir, {github,Repo}, State) ->
  rebar_git_resource:download(Dir, {git,?GH(Repo)}, State).

cached_download(TmpDir, CachePath, {github,Repo,{tag,Vsn}}=Dep, Url, ETag, State) ->
  %% Start with head, since GitHub doesn't seem to respect If-None-Match.
  case request(head, Url, ETag) of
    {ok,cached} ->
      ?INFO("Version cached at ~s is up to date, reusing it", [CachePath]),
      serve_from_cache(TmpDir, tar_path(CachePath, ETag), Dep, State);
    {ok,Body,NewETag} ->
      ?INFO("Downloaded package, caching at ~s", [CachePath]),
      serve_from_download(TmpDir, CachePath, Dep, NewETag, Body, State);
    error when ETag =/= false ->
      ?INFO("Download error, using cached file at ~s", [CachePath]),
      serve_from_cache(TmpDir, tar_path(CachePath, ETag), Dep, State);
    error ->
      {fetch_fail,Repo,Vsn}
  end.

serve_from_cache(TmpDir, TarPath, _Dep, _State) -> extract(TmpDir, TarPath).

serve_from_download(TmpDir, CachePath, GitHubDep, ETag, Binary, State) ->
  filelib:ensure_dir(CachePath),
  lists:foreach(fun file:delete/1, filelib:wildcard(CachePath++"*")),
  TarPath = tar_path(CachePath, ETag),
  ?DEBUG("Writing ~p to cache at ~s", [GitHubDep,TarPath]),
  file:write_file(TarPath, Binary),
  case etag(TarPath) of
    ETag ->
      serve_from_cache(TmpDir, TarPath, GitHubDep, State);
    FileETag ->
      ?DEBUG("Downloaded file ~s ETag ~s doesn't match returned ETag ~s",
             [TarPath,ETag,FileETag]),
      {bad_download,TarPath}
  end.

-spec tar_path(CachePath::string(), ETag::string()) -> TarPath::string().
tar_path(CachePath, ETag) -> CachePath ++ "-" ++ ETag ++ ".tar.gz".

-spec extract(string(), string()) -> {ok,true} | {failed_extract,string()}.
extract(TmpDir, TarPath) ->
  ec_file:mkdir_p(TmpDir),
  ok = erl_tar:extract(TarPath, [{cwd,TmpDir},compressed]),
  {NameVsn,_} = namevsn_etag(TarPath),
  ExtDir = filename:join(TmpDir, NameVsn),
  case move_files(TmpDir, filelib:wildcard(filename:join(ExtDir, "*"))) of
    ok ->
      ok = file:del_dir(ExtDir),
      {ok,true};
    {error,_Reason} ->
      {failed_extract,TarPath}
  end.

-spec move_files(TmpDir::string(), Files::[string()]) -> ok | {error,atom()}.
move_files(TmpDir, [File|Files]) ->
  Dest = filename:join(TmpDir, filename:basename(File)),
  case file:rename(File, Dest) of
    ok    -> move_files(TmpDir, Files);
    Error -> Error
  end;
move_files(_TmpDir, []) -> ok.

-spec make_vsn(file:filename_all()) -> {error,string()}.
make_vsn(_Dir) -> {error,"Replacing version of type github not supported."}.

-spec request(get | head, Url::string(), ETag::string()) -> Result when
    Result :: {ok,cached} | {ok,Body::binary(),ETag::string()} | error.
request(Method, Url, ETag) ->
  case httpc:request(Method, {Url,[{"User-Agent",rebar_utils:user_agent()}]},
                     [{ssl,rebar_pkg_resource:ssl_opts(Url)},{relaxed,true}],
                     [{body_format,binary}],
                     rebar) of
    {ok,{{_Version,200,_Reason},Headers,Body}} ->
      case lists:keyfind("etag", 1, Headers) of
        {"etag",ETag1} when head =:= Method ->
          ETag2 = string:strip(ETag1, both, $"),
          case ETag =:= ETag2 of
            true ->
              ?DEBUG("Cached copy of ~s still valid", [Url]),
              {ok,cached};
            false ->
              request(get, Url, ETag)
          end;
        {"etag",ETag1} when get =:= Method ->
          ?DEBUG("Successfully downloaded ~s", [Url]),
          {ok,Body,string:strip(ETag1, both, $")}
      end;
    {ok,{{_Version,Code,_Reason},_Headers,_Body}} ->
      ?DEBUG("Request to ~p failed: status code ~p", [Url,Code]),
      error;
    {error,Reason} ->
      ?DEBUG("Request to ~p failed: ~p", [Url,Reason]),
      error
  end.

-spec etag(CachePath::string()) -> ETag::string() | false.
etag(CachePath) ->
  case filelib:wildcard(CachePath ++ "*") of
    [Path|_] ->
      {_,ETag} = namevsn_etag(Path),
      ETag;
    []       -> false
  end.

-spec namevsn_etag(TarPath::string()) -> {NameVsn::string(),ETag::string()}.
namevsn_etag(TarPath) ->
  NameVsnETag = filename:rootname(filename:basename(TarPath), ".tar.gz"),
  ETag = lists:last(string:tokens(NameVsnETag, "-")),
  {filename:basename(NameVsnETag, [$-|ETag]),ETag}.
