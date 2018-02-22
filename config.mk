HOST := $(shell uname -m)

PPC-x86_64 = ppc-amd64
PPC-arm = ppc-arm
PPC-mips = ppc-mips

PPC := $(PPC-$(HOST))
AS_FLAGS := $(AS_FLAGS-$(HOST))

ifndef PPC
    $(error Can't configure for host type $(HOST))
endif

