RMD_DIR := Rmd
OUT_DIR := docs
RMD     := $(wildcard $(RMD_DIR)/*.Rmd)
HTML    := $(patsubst $(RMD_DIR)/%.Rmd,$(OUT_DIR)/%.html,$(RMD))
PDF     := $(patsubst $(RMD_DIR)/%.Rmd,$(OUT_DIR)/%.pdf,$(RMD))

all: $(HTML) $(PDF)

$(OUT_DIR)/%.html: $(RMD_DIR)/%.Rmd | $(OUT_DIR)
	Rscript -e "rmarkdown::render('$<', output_format='html_document', output_dir='$(OUT_DIR)')"

$(OUT_DIR)/%.pdf: $(RMD_DIR)/%.Rmd | $(OUT_DIR)
	Rscript -e "rmarkdown::render('$<', output_format='beamer_presentation', output_dir='$(OUT_DIR)')"

$(OUT_DIR):
	mkdir -p $(OUT_DIR)

clean:
	rm -f $(OUT_DIR)/*.{html,pdf}

.PHONY: all clean