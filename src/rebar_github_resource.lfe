(defmodule rebar_github_resource
  (export (lock 2) (download 3) (needs_update 2) (make_vsn 1)))

(defmacro DEBUG (str args) `(rebar_api:debug ,str ,args))
(defmacro INFO  (str args) `(rebar_api:info  ,str ,args))
(defmacro WARN  (str args) `(rebar_api:warn  ,str ,args))

(defmacro GH (repo) `(++ "https://github.com/" ,repo))

(defun lock
  ([_app-dir `#(github ,repo #(tag ,vsn))]
   `#(github ,(filename:rootname repo ".git") #(tag ,vsn)))
  ([app-dir `#(github ,repo ,other)]
   (rebar_git_resource:lock app-dir `#(git ,(GH repo) ,other)))
  ([app-dir `#(github ,repo)]
   (rebar_git_resource:lock app-dir `#(git ,(GH repo)))))

(defun needs_update
  ([dir `#(github ,_repo #(tag ,vsn))]
   (let ((`[,app-info] (rebar_app_discover:find_apps `[,dir] 'all)))
     (=/= (rebar_app_info:original_vsn app-info) (ec_cnv:to_list vsn))))
  ([dir `#(github ,repo ,other)]
   (rebar_git_resource:needs_update dir `#(git ,(GH repo) ,other)))
  ([dir `#(github ,repo)]
   (rebar_git_resource:needs_update dir `#(git ,(GH repo)))))

(defun download
  ([dir (= `#(github ,repo #(tag ,vsn)) dep) state]
   (let* ((repo* (filename:rootname repo ".git"))
          (rebar-opts (rebar_state:opts state))
          (global-cache-dir (rebar_dir:global_cache_dir rebar-opts))
          (`[,user ,name] (filename:split repo*))
          ;; TODO: make this configurable
          (cache-dir (filename:join `[,global-cache-dir "github" ,user]))
          (archive (++ vsn ".tar.gz"))
          (cache-path (filename:join cache-dir (++ name "-" vsn)))
          (archive-path (filename:join "archive" archive)))
     (case (rebar_utils:url_append_path (GH repo*) archive-path)
       (`#(ok ,url)
        (cached-download dir cache-path dep url (etag cache-path) state))
       (_
        `#(fetch_fail ,repo* ,vsn)))))
  ([dir `#(github ,repo ,other) state]
   (rebar_git_resource:download dir `#(git ,(GH repo) ,other)) state)
  ([dir `#(github ,repo) state]
   (rebar_git_resource:download dir `#(git ,(GH repo)) state)))

(defun cached-download
  ([tmp-dir cache-path (= `#(github ,repo #(tag ,vsn)) dep) url etag state]
   ;; Start with head, since GitHub doesn't seem to respect If-None-Match.
   (case (request 'head url etag)
     (#(ok cached)
      (INFO "Version cached at ~s is up to date. Reusing it." `[,cache-path])
      (extract tmp-dir (tar-path cache-path etag)))
     (`#(ok ,body ,etag*)
      (INFO "Downloaded archive. Caching at ~s." `[,cache-path])
      (serve-from-download tmp-dir cache-path dep etag* body state))
     ('error (when (=/= 'false etag))
             (INFO "Download error. Using cached file at ~s." `[,cache-path])
             (extract tmp-dir (tar-path cache-path etag)))
     ('error
      `#(fetch_fail ,repo ,vsn)))))

(defun serve-from-download (tmp-dir cache-path dep etag binary state)
  (filelib:ensure_dir cache-path)
  (lists:foreach #'file:delete/1 (filelib:wildcard (++ cache-path "*")))
  (let ((tar-path (tar-path cache-path etag)))
    (DEBUG "Writing ~p to cache at ~s." `[,dep ,tar-path])
    (file:write tar-path binary)
    (extract tmp-dir tar-path)))

(defun tar-path (cache-path etag) (++ cache-path "-" etag ".tar.gz"))

(defun extract (tmp-dir tar-path)
  (ec_file:mkdir_p tmp-dir)
  (let (('ok (erl_tar:extract tar-path `[#(cwd ,tmp-dir) compressed]))))
  (let* ((`#(,namevsn ,_) (namevsn-etag tar-path))
         (ext-dir (filename:join tmp-dir namevsn)))
    (case (move-files tmp-dir (filelib:wildcard (filename:join ext-dir "*")))
      ('ok
       (let (('ok (file:del_dir ext-dir))))
       #(ok true))
      (`#(error ,_reason)
       `#(failed_extract ,tar-path)))))

(defun move-files
  ([tmp-dir `[,file . ,files]]
   (case (file:rename file (filename:join tmp-dir (filename:basename file)))
     ('ok   (move-files tmp-dir files))
     (error error)))
  ([_tmp-dir ()] 'ok))

(defun make_vsn (_dir)
  #(error "Replacing version of type github not supported."))

(defun request (method url etag)
  (case (httpc:request method
                       `#(,url [#("User-Agent" ,(rebar_utils:user_agent))])
                       `[#(ssl ,(rebar_pkg_resource:ssl_opts url))
                         #(relaxed true)]
                       '[#(body_format binary)]
                       'rebar)
    (`#(ok #((,_version 200 ,_reason) ,headers ,body))
     (case (lists:keyfind "etag" 1 headers)
       (`#("etag" ,etag*) (when (=:= 'head method))
        (let ((etag** (string:strip etag* 'both #\")))
          (if (=:= etag etag**)
            (progn (DEBUG "Cached copy of ~s still valid." `[,url])
                   #(ok cached))
            (request 'get url etag))))
       (`#("etag" ,etag*) (when (=:= 'get method))
        (DEBUG "Successfully downloaded ~s." `[,url])
        `#(ok ,body ,(string:strip etag* 'both #\")))
       (`#(ok #((,_version ,code ,_reason) ,_headers ,_body))
        (DEBUG "Request to ~p failed: status code ~p." `[,url ,code])
        'error)
       (`#(error ,reason)
        (DEBUG "Request to ~p failed: ~p" `[,url ,reason])
        'error)))))

(defun etag (cache-path)
  (case (filelib:wildcard (++ cache-path "*"))
    (`[,path . ,_] (let ((`#(,_ ,etag) (namevsn-etag path))) etag))
    ([]            'false)))

(defun namevsn-etag (tar-path)
  (let* ((nve (filename:rootname (filename:basename tar-path) ".tar.gz"))
         (etag (lists:last (string:tokens nve "-"))))
    `#(,(filename:basename nve `[#\- . ,etag]) ,etag)))
