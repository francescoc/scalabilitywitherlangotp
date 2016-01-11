{application, bsc,
   [{description, "Base Station Controller"},
    {vsn, "1.0"},
    {modules, [bsc, bsc_sup, frequency, freq_overload, logger, simple_phone_sup, phone_fsm]},
    {registered, [bsc_sup, frequency, frequency_sup, overload, simple_phone_sup]},
    {applications, [kernel, stdlib, sasl]},
    {env, []},
    {mod, {bsc, []}}]}.
