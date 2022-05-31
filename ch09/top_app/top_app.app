{application, top_app,
 [{description, "Included Application Example"},
  {vsn, "1.0"},
  {modules, [top_app]},
  {applications, [kernel, stdlib, sasl]},
  {included_applications, [bsc]},
  {start_phases, [{start, []}, {admin, []}, {stop, []}]},
  {mod, {application_starter, [top_app, []]}}
 ]
}.
