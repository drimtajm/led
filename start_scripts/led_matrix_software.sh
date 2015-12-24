#! /bin/sh
### BEGIN INIT INFO
# Provides:          Led matrix software
# Required-Start:
# Required-Stop:
# Default-Start:     S
# Default-Stop:
# Short-Description: Start led matrix controller node and yaws
# Description:       Start an Erlang node that runs led matrix controller code
#                    and the yaws web server (which will start another node)
### END INIT INFO

RPI_HW_DRIVERS_EBIN_DIR=/home/pi/workspace/erlang-rpi-hw-drivers/ebin
LED_MATRIX_CODE_EBIN_DIR=/home/pi/workspace/led/erlang
MATRIX_MODULE=led_array
YAWS_ARGS="--sname yaws --setcookie \"erlang-rocks\"" 

led_matrix_controller_start () {
    erl -pa $RPI_HW_DRIVERS_EBIN_DIR -pa $LED_MATRIX_CODE_EBIN_DIR \
	-sname led_server -setcookie "erlang-rocks" -noinput \
	-s $MATRIX_MODULE go >> /var/log/led_matrix_controller.log &
}

yaws_start() {
    yaws -D $YAWS_ARGS
}

###########

erl_stop_node () {
   erl -noshell -sname temp_control \
	-eval "{ok, Hostname} = inet:gethostname(), Nodename = lists:concat([\"$1@\", Hostname]), rpc:call(list_to_atom(Nodename), init, stop, [])" \
	-s init stop -setcookie "erlang-rocks"
}

led_matrix_controller_stop () {
    erl_stop_node led_server
}

yaws_stop() {
    erl_stop_node yaws
}

###########

case "$1" in
    start|"")
        yaws_start
	led_matrix_controller_start
	;;
    restart)
	led_matrix_controller_stop
        /usr/local/bin/yaws -h $YAWS_ARGS
	led_matrix_controller_start
	;;
    stop)
	led_matrix_controller_stop
	yaws_stop
	;;
    *)
	echo "Usage: $0 [start|stop|restart]" >&2
	exit 1
	;;
esac

exit 0
