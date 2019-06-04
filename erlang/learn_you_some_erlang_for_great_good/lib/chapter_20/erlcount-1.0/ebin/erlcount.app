{application, erlcount,
  [
    {vsn, "1.0.0"},
    {modules, [erlcount, erlcount_sup, erlcount_lib, erlcount_dispatch, erlcount_sup]},
    {applications, [ppool]},
    {registered, [erlcount]},
    {mod, {erlcount, []}},
    {env, [
      {directory, "."},
      {regex, ["if\\s.+->", "case\\s.x\\sof"]},
      {max_files, 10}
    ]}
  ]
}.