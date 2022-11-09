HOST := $(shell uname -m)

PPC-x86_64 = ppc-amd64
PPCFLAGS-x86_64 = -pic

PPC-armv7l = ppc-arm
ASFLAGS-armv7l = -march=armv6

PPC-aarch64 = ppc-arm64

PPC-mips = ppc-mips
ASFLAGS-mips = -msoft-float

PPC := $(PPC-$(HOST))
PPCFLAGS := $(PPCFLAGS-$(HOST))
ASFLAGS := $(ASFLAGS-$(HOST))

ifndef PPC
    $(error Can't configure for host type $(HOST))
endif

