version 1.0

#WDL example
#reference_genome_fa gc://gcp-public-data--broad-references/hg38/v0/Homo_sapiens_assembly38.fasta
#telseq_docker_image gcr.io/ukbb-analyses/telseq


workflow SigPro {
  input {
    File vcf_zip
  }


  call SigProfAss {
      input:
      vcf_zip = vcf_zip
  }

  
  output {
    File sigproout = SigProfAss.output_tar
  }
}


task SigProfAss {
    input {
      File vcf_zip
      #Runtime parameters
      Int additional_disk_size = 3
      Int mem = 8
      Int preemptible_tries = 3
    }

    Float input_size = size(vcf_zip, "GB")
    #assume unzip inflates 2x, output size is similar to input
    Int disk_size = ceil(input_size * 5) + additional_disk_size
    String name = basename(vcf_zip, ".zip")

    #Calls samtools view to do the conversion
    command <<<
      #Set -e and -o says if any command I run fails in this script, make sure to return a failure
      set -e
      set -o pipefail

      #unzip input folder
      unzip -d vcf_fld ~{vcf_zip} 

      #script
      wget -O sigprofiler.py https://raw.githubusercontent.com/tetnakao/sigprofiler/main/SigProfilerAssignment.py

      #production
      python sigprofiler.py

      #from SigProfilerAssignment import Analyzer as Analyze
      #Analyze.cosmic_fit(samples="vcf_fld", output="out", input_type="vcf", context_type="96",
      #             collapse_to_SBS96=True, cosmic_version=3.3, exome=False,
      #             genome_build="GRCh38", signature_database=None,
      #             exclude_signature_subgroups=None, export_probabilities=True,
      #             export_probabilities_per_mutation=True, make_plots=True,
      #             sample_reconstruction_plots=True, verbose=True)

      #zip
      tar -czfv ~{name}.tar.gz out

    >>>

    runtime {
        docker: "tetsushinakao/sigprofass:bash.38"
        memory: mem + " GB"
        disks: "local-disk " + disk_size + " HDD"
        preemptible: preemptible_tries
    }

    output {
        File output_tar = "~{name}.tar.gz"
    }
}