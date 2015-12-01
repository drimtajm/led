1. Install Erlang and Yaws
2. Download and compile https://github.com/drimtajm/erlang-rpi-hw-drivers
3. Set environment variable RPI_HW_DRIVERS_EBIN_DIR to the ebin directory from the previous step
4. Copy files in the yaws directory to /usr/local/var/yaws/www/led (or other location according to your settings)
5. Adapt the page according to your needs (i.e. double matrix or several colours)
6. Make sure hostname is set and inet:gethostname() returns it
7. Compile the files in the erlang directory
8. Use the scripts in the start_scripts directory to start yaws and a led_node
9. Start matrix code (e.g. by calling "double_matrix:go().") in the led_node shell
10. Surf to http://<your ip>/led/test2.html and play around
