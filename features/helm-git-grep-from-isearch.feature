Feature: Search a keyword from isearch
  In order to search a keyword
  As a user
  I want to grep the keyword by helm-git-grep-from-isearch

  Background: I open diff-tree.c
    Given I open file "diff-tree.c"
    Then I should be in buffer "diff-tree.c"
    When I go to point "4060"

  Scenario: I grep a keyword by helm-git-grep-from-isearch
    Given I start an action chain
    And I press "C-s"
    When I press "C-w"
    When I press "C-w"
    And I execute the action chain
    Given I start an action chain
    When I press "C-c g"
    When I press "RET"
    When I press "RET"
    And I execute the action chain
    Then I should be in buffer matching regexp "diff-files.c"
    Then the cursor should be at line "11"
