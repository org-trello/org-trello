# tools
# set TexInfoDir to location of Texinfo commands or leave blank if in PATH
TexInfoDir := /usr/local/Cellar/texinfo/6.5/bin/

# installation
# set InfoInstallDir to desired location of .info in INFOPATH. Must contain "dir" file.
InfoInstallDir := ~/info

# Source
BaseName := org-trello
OrgSrc := $(BaseName).org
TexiSrc := $(BaseName).texi
TexiInc := gpl.texi fdl.texi

Objects := $(BaseName).aux $(BaseName).cp $(BaseName).cps \
	$(BaseName).fn $(BaseName).fns $(BaseName).ky $(BaseName).kys \
	$(BaseName).log $(BaseName).toc $(BaseName).vr $(BaseName).vrs

# Output
InfoOut := $(BaseName).info
InfoGzOut := $(BaseName).info.gz
HtmlOut := $(BaseName).html
PdfOut := $(BaseName).pdf
Outputs := $(InfoOut) $(InfoGzOut) $(HtmlOut) $(PdfOut)

info : $(InfoOut)

.PHONY : all view info html pdf install clean cleanall

all  : info html pdf

install : $(InfoOut)
	rm $(InfoGzOut) || True
	gzip -k $<
	install $(InfoGzOut) $(InfoInstallDir)
	install-info $(InfoInstallDir)/$(InfoGzOut) $(InfoInstallDir)/dir

view : $(InfoOut)
	$(TexInfoDir)info ./$<

$(InfoOut) : $(TexiSrc) $(TexiInc) README.org
	$(TexInfoDir)makeinfo $<

html : $(TexiSrc) info images/texinfo.png
	$(TexInfoDir)texi2any --html --no-split $<

pdf : $(TexiSrc) info images/texinfo.png
	$(TexInfoDir)texi2pdf $<

clean :
	rm $(Objects) || True

cleanall : clean
	rm $(Outputs) || True
