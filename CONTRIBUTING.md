# Contributing to `eva3dm`

Thank you for any and all contributions! Following these guidelines will help streamline the process of contributing and make sure that we're all on the same page. While we ask that you read this guide and follow it to the best of your abilities, we welcome contributions from all, regardless of your level of experience.

By participating in this project, you agree to abide by the [code of conduct](https://github.com/schuch666/eva3dm/blob/JOSS/CODE_OF_CONDUCT.md).

# Types of contributions

Don't feel that you must be a computer whiz to make meaningful contributions. Feel free to:

- Identify areas for future development ([open an Issue](https://github.com/schuch666/eva3dm/issues))
- Identify issues/bugs ([open an Issue](https://github.com/schuch666/eva3dm/issues))
- Write tutorials/vignettes ([open a Pull Request](https://github.com/schuch666/eva3dm/pulls) to contribute to the ones here, or make your own elsewhere and send us a link)
- Add functionality ([open a Pull Request](https://github.com/schuch666/eva3dm/pulls))
- Fix bugs ([open a Pull Request](https://github.com/schuch666/eva3dm/pulls))

# New to GitHub?

Getting ready to make your first contribution? Here are a couple of tutorials you may wish to check out:

- [Tutorial for first-timers](https://github.com/Roshanjossey/first-contributions)
- [GitHub on setup](https://help.github.com/articles/set-up-git)
- [GitHub on pull requests](https://help.github.com/articles/using-pull-requests/).


# How to contribute code

- Fork the repository
- Clone the repository from GitHub to your computer e.g,. `git clone https://github.com/schuch666/eva3dm.git`
- Make sure to track progress upstream (i.e., on our version of `eva3dm` at `schuch666/eva3dm`)
  - `git remote add upstream https://github.com/schuch666/eva3dm.git`
  - Before making changes make sure to pull changes in from upstream with `git pull upstream`
- Make your changes
  - For changes beyond minor typos, add an item to NEWS.md describing the changes and add yourself to the DESCRIPTION file as a contributor
- Push to your GitHub account
- Submit a pull request to home base (likely master branch, but check to make sure) at `schuch666/eva3dm`

# Code formatting

- In general follow the convention of <https://r-pkgs.org/code.html#code-style> (snake_case functions and argument names, etc.)
- Where there is conflict, default to the style of `eva3dm`
- Use explicit package imports (i.e. package_name::package_function) and avoid @import if at all possible
