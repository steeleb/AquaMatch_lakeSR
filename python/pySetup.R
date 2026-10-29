library(reticulate)

# NOTE, IF YOU ALTER THIS SCRIPT, YOU WILL NEED TO DELETE THE "env" FOLDER
# SO THAT CHANGES ARE REBUILT IN A NEW ENVIRONMENT

# Python and package versions for the conda environment. Leave a version as ""
# to let conda choose the latest compatible version (only for a fresh install
# when updating versions); otherwise pin every version so the environment is
# reproducible. After an unpinned install, the installed versions are printed
# in this format to copy back in here.
py_versions <- c(
  "python" = "3.12.15",
  "earthengine-api" = "1.7.48",
  "pandas" = "3.0.6",
  "numpy" = "2.5.3",
  "pyyaml" = "6.0.3"
)

env_path <- file.path(getwd(), "env/")

# conda's single "=" matches as a prefix (e.g. "3.12" matches 3.12.x)
py_specs <- ifelse(py_versions == "",
                   names(py_versions),
                   paste0(names(py_versions), "=", py_versions))

# By default, conda-forge's numpy uses OpenBLAS, which loads its own OpenMP
# runtime. On macOS, R's {data.table} (a dependency of {targets}) also loads
# R's OpenMP runtime, and two OpenMP runtimes in one R session causes
# "OMP: Error #15" or a fatal crash of the R session. On macOS, we use Apple's
# Accelerate framework for BLAS instead, which does not use OpenMP.
if (Sys.info()[["sysname"]] == "Darwin") {
  py_specs <- c(py_specs, "libblas=*=*accelerate")
} else {
  message("This Python environment was built and tested on macOS. On ",
          Sys.info()[["sysname"]], ", you may need to troubleshoot the ",
          "environment set up. If your R session crashes or you see ",
          "'OMP: Error #15' when using Python, the conda environment and R are ",
          "loading separate copies of the OpenMP runtime.")
}

# create the conda environment if needed. To keep set up fast, all packages are
# installed in a single solve, using only the conda-forge channel and the
# libmamba solver (much faster than conda's "classic" solver)
if (!dir.exists(env_path)) {
  try(install_miniconda(), silent = TRUE)
  create_args <- c("--override-channels", "--strict-channel-priority")
  tryCatch(
    conda_create(envname = env_path, packages = py_specs,
                 channel = "conda-forge",
                 additional_create_args = c(create_args, "--solver", "libmamba")),
    error = function(e) {
      # older conda installations may not include the libmamba solver
      message("libmamba solver unavailable, using conda's default solver.")
      unlink(env_path, recursive = TRUE)
      conda_create(envname = env_path, packages = py_specs,
                   channel = "conda-forge",
                   additional_create_args = create_args)
    }
  )
  # report installed versions so unpinned versions can be pinned above
  installed <- py_list_packages(envname = env_path, type = "conda")
  installed <- installed[installed$package %in% names(py_versions), ]
  message("Installed versions (copy into py_versions in python/pySetup.R):\n",
          paste0('  "', installed$package, '" = "', installed$version, '"',
                 collapse = ",\n"))
}

# activate the environment. reticulate can only bind to one Python per R
# session, so this fails if a different Python has already been initialized
tryCatch(
  use_condaenv(env_path, required = TRUE),
  error = function(e) {
    message(conditionMessage(e))
    stop("There was an error initializing Python. Restart your R session and try again.")
  }
)

# check that the environment is complete (e.g. set up was not interrupted)
if (!py_module_available("ee")) {
  stop("The conda environment is incomplete. Restart your R session, delete the ",
       "'env' folder, and run this script again.")
}

message("conda environment activated")
