{application, simple_messaging_scalaris,
 [{description, "simple_messaging_scalaris a mochiweb to scalaris example"},
  {vsn, "0.01"},
  {modules, [
    scalaris_proxy,
    simple_messaging_scalaris,
    simple_messaging_scalaris_app,
    simple_messaging_scalaris_sup,
    simple_messaging_scalaris_web,
    simple_messaging_scalaris_deps
  ]},
  {registered, []},
  {mod, {simple_messaging_scalaris_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
