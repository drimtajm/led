sudo LC_TYPE=sv_SE.UTF-8 erl -pa "erlang/ebin" -pa "erlang/deps/erlang-rpi-hw-drivers/ebin" -sname led_server -matrix_controller display_type double_matrix -setcookie "erlang-rocks"
