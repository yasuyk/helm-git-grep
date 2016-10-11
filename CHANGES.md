# 0.10.0-snapshot (in development)

- TODO for release 0.10.0
    - Update gif
    - Update README.md

- New features:
    - [\#11](https://github.com/yasuyk/helm-git-grep/issues/11 "#11") [\#23](https://github.com/yasuyk/helm-git-grep/issues/11 "#23") [\#26](https://github.com/yasuyk/helm-git-grep/issues/11 "#26") Can change base directory where on git-grep(1) is executed
    - Can limit paths in git-grep(1) by [pathspec]

- Bug fixes:
    - Fix that helm-git-grep-save-results can't work([d49001e])

# 0.9.0 (Oct 10, 2016)

- New features:
    - [\#25](https://github.com/yasuyk/helm-git-grep/issues/25 "#25") Make helm sources customizable

# 0.8.0 (Jul 13, 2016)

- New features:
    - Support latest helm
      Use `helm-source-async` instead of `define-helm-type-attribute`

# 0.7.1 (Apr 9, 2016)

- Bug fixes:

    - Don't match space separate terms ([1e0357f])

# 0.7 (Apr 3, 2016)

- New features:

    - Add customizable faces


[pathspec]:https://git-scm.com/docs/gitglossary#def_pathspec
[1e0357f]:https://github.com/yasuyk/helm-git-grep/commit/1e0357f
[d49001e]:https://github.com/yasuyk/helm-git-grep/commit/d49001e
