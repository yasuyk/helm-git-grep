Feature: Search a keyword at point
  In order to search a keyword
  As a user
  I want to grep the keyword by helm-git-grep-at-point

  Background: I open a file
    Given I open file "config.c"
    Then I should be in buffer "config.c"
    When I go to point "8153"

  Scenario: I grep a keyword by helm-git-grep-at-point
    Given I start an action chain
    And I press "M-x"
    And I type "helm-git-grep-at-point"
    When I press "RET"
    When I press "RET"
    And I execute the action chain
    Then I should be in buffer matching regexp "cache.h"
    Then the cursor should be at line "330"
