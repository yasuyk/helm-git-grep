Feature: Search a keyword and save search results
  In order to search a keyword
  As a user
  I want to grep the keyword and save search results

  Scenario: I save search results
    Given I start an action chain
    And I press "M-x"
    And I type "helm-git-grep"
    When I press "RET"
    When I type "get_commit_format"
    And I press "TAB"
    And I press "C-n"
    And I press "C-n"
    And I press "C-n"
    And I execute the action chain
    Then I should be in buffer matching regexp "\*hggrep\*"
    Then the cursor should be at line "5"
