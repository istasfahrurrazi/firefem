all:
	gfortran-mp-6  -fdollar-ok \
                automeshview.for \
                design13.for \
                fin92.for \
                general.for \
                printing.for \
                sorting.for \
                temperatureprofile.for\
        -o firefem


