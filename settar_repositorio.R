library(usethis)

library(gert)

# Iniciar git ----

usethis::use_git()

# Primeiro commit ----

gert::git_add(".gitignore")

gert::git_commit("gitignore")

# Criar repositório ----

usethis::use_github()

# Criar README ----

usethis::use_readme_md()
