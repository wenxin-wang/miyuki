INSTALL_BIN_DIR ?= ~/.local/bin

all:

install:
	mkdir -p ${INSTALL_BIN_DIR}
	install -m 755 miyuki ${INSTALL_BIN_DIR}/miyuki

uninstall:
	rm ${INSTALL_BIN_DIR}/miyuki 2>/dev/null || :
