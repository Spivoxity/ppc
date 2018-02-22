HOST := $(shell uname -m)

PPC-x86_64 = ppc-amd64
PPCFLAGS-x86_64 = -pic

PPC-armv7l = ppc-arm
ASFLAGS-armv7l = -march=armv6

PPC-mips = ppc-mips

PPC := $(PPC-$(HOST))
PPCFLAGS := $(PPCFLAGS-$(HOST))
ASFLAGS := $(ASFLAGS-$(HOST))

ifndef PPC
    $(error Can't configure for host type $(HOST))
endif

