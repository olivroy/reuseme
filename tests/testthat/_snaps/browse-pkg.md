# browse_pkg() works

    Code
      browse_pkg("cli", open = FALSE)
    Message
      
      -- cli (<https://cli.r-lib.org>) --
      
      news (<https://cli.r-lib.org/news>)
      articles (<https://cli.r-lib.org/articles>)
      dev (<https://cli.r-lib.org/dev>)
      reference (<https://cli.r-lib.org/reference>)
      cran (<https://cran.r-project.org/package=cli>)
      github (<https://github.com/r-lib/cli>)
      
    Code
      browse_pkg("reuseme", news_only = TRUE)
    Output
      <cli_ansi_string>
      [1] news
    Code
      browse_pkg("reuseme", ref_only = TRUE)
    Output
      [1] "https://olivroy.github.io/reuseme/reference"

