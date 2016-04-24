# rebar3\_github\_cache

_A `rebar3` plugin enabling brevity and caching for GitHub deps._


## Installation

```erlang
{plugins, [
  {rebar3_github_cache,
   {git, "https://github.com/yurrriq/rebar3_github_cache.git",
    {tag, "0.1.0"}}}
]}.
```


## Usage

```erlang
{deps, [
  %% Defaults to normal git dependency
  {lfe,       {github, "rvirding/lfe",           {branch, "develop"}}},

  %% Cached
  {clj,       {github, "lfex/clj",               {tag, "0.4.1"}}},
  {exemplar,  {github, "lfex/exemplar",          {tag, "0.4.1"}}},
  {levaindoc, {github, "quasiquoting/levaindoc", {tag, "0.5.0"}}}
]}.
```


## Caching

```
~/.cache/rebar3/github/{{user}}/{{repo}}-{{tag}}-{{etag}}.tar.gz
```

Where `etag` is the `ETag` reported by GitHub in the HTTP response for the
tarball.

N.B. This is **not** an MD5 or SHA checksum of tarball itself.


## License

Apache
