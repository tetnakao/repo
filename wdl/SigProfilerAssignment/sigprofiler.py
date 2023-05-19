from SigProfilerAssignment import Analyzer as Analyze
Analyze.cosmic_fit(samples="vcf_fld", output="out", input_type="vcf", context_type="96",
             collapse_to_SBS96=True, cosmic_version=3.3, exome=False,
             genome_build="GRCh38", signature_database=None,
             exclude_signature_subgroups=None, export_probabilities=True,
             export_probabilities_per_mutation=True, make_plots=True,
             sample_reconstruction_plots=True, verbose=True)
