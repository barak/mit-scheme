## Process this file with automake to produce Makefile.in

TARGET_ROOT = mit-scheme-ref

info_TEXINFOS = mit-scheme-ref.texi

mit_scheme_ref_TEXINFOS = \
	acknowledgements.texi \
	associations.texi \
	binding-index.texi \
	bit-strings.texi \
	characters.texi \
	concept-index.texi \
	environments.texi \
	equivalence.texi \
	error.texi \
	gfdl.texinfo \
	graphics.texi \
	io.texi \
	lists.texi \
	misc-datatypes.texi \
	numbers.texi \
	os-interface.texi \
	overview.texi \
	procedures.texi \
	special-forms.texi \
	strings.texi \
	vectors.texi \
	$(STD_TARGETS)

# The standards chapter is generated from the same description of the
# standard libraries that the runtime is built from.  Both are shipped in
# the distribution, so only a build from git needs the Scheme to do it.
STD_TARGETS = standards.texi standards-menu.texi
STDDIR = $(top_srcdir)/../etc/standards
$(STD_TARGETS): $(STDDIR)/standards.scm $(STDDIR)/generate-standards-doc.sh
	$(AM_V_GEN)$(STDDIR)/generate-standards-doc.sh $@

FIGURES_EPS = \
	fig/cn-expm1.eps \
	fig/cn-exsec.eps \
	fig/cn-log1mexp.eps \
	fig/cn-log1p.eps \
	fig/cn-log1pexp.eps \
	fig/cn-logistic.eps \
	fig/cn-logistichalf.eps \
	fig/cn-logit.eps \
	fig/cn-logitexp.eps \
	fig/cn-logithalf.eps \
	fig/cn-loglogistic.eps \
	fig/cn-sqrt1pm1.eps \
	fig/cn-versin.eps \
	fig/expm1.eps \
	fig/exsec.eps \
	fig/log1mexp.eps \
	fig/log1p.eps \
	fig/log1pexp.eps \
	fig/logistic.eps \
	fig/logistichalf.eps \
	fig/logit.eps \
	fig/logitexp.eps \
	fig/logithalf.eps \
	fig/loglogistic.eps \
	fig/sqrt1pm1.eps \
	fig/versin.eps

FIGURES_PNG = $(FIGURES_EPS:.eps=.png)
FIGURES_PDF = $(FIGURES_EPS:.eps=.pdf)

FIGURES_COMMON = \
	fig/common/ad.ps \
	fig/common/math.ps \
	fig/common/newton.ps \
	fig/common/plot.ps \
	fig/common/splinterp.ps

GS = gs
EPS2EPS = eps2eps
PS2PDF = ps2pdf
PS2PNG = $(GS) -dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -r300
EPS2EPSFLAGS = -Ifig

# The Info and HTML forms embed the PNGs, TeX wants the PDFs.  These go in
# _TEXINFOS rather than in per-output rules: naming a target that automake
# also generates makes automake drop its own rule for it, which leaves the
# output with prerequisites and no recipe.
mit_scheme_ref_TEXINFOS += $(FIGURES_PNG) $(FIGURES_PDF)
TEXI_FIGURES = $(FIGURES_PNG)

$(FIGURES_EPS): $(FIGURES_COMMON)

SUFFIXES = .eps.in .eps .pdf .png

.eps.in.eps:
	$(AM_V_GEN)$(MKDIR_P) fig && \
	$(EPS2EPS) $(EPS2EPSFLAGS) $< $@.tmp && mv -f $@.tmp $@

.eps.pdf:
	$(AM_V_GEN)$(PS2PDF) -dEPSCrop -sOutputFile=$@.tmp $< && mv -f $@.tmp $@

.eps.png:
	$(AM_V_GEN)$(PS2PNG) -dEPSCrop -sOutputFile=$@.tmp $< && mv -f $@.tmp $@

CLEANFILES = $(FIGURES_EPS) $(FIGURES_PNG) $(FIGURES_PDF) $(STD_TARGETS)

EXTRA_DIST = tools.scm $(FIGURES_COMMON) $(FIGURES_EPS:.eps=.eps.in)

include $(top_srcdir)/texinfo.am
