-record(register_value, {register,
			 value,
			 add = false}).
-record(configuration, {ce_pin = 8,
			initial_register_values = [],
			%% Display type is: double_matrix or colour_matrix
			display_type}).
-record(double_matrix_row, {left_value = 0,
			    right_value = 0}).
-record(colour_matrix_row, {red_value = 0,
			    green_value = 0}).
-record(controller_state, {spi_handle,
			   configuration,
			   code}).

