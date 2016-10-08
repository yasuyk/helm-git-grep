Feature: Search a keyword
  In order to search a keyword
  As a user
  I want to grep the keyword by helm-git-grep

  Scenario: I grep a keyword by helm-git-grep
    Given I start an action chain
    And I press "M-x"
    And I type "helm-git-grep"
    When I press "RET"
    When I type "get_commit_format"
    When I press "RET"
    And I execute the action chain
    Then I should be in buffer matching regexp "commit.c"
    Then the cursor should be at line "25"
