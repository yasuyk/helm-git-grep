Feature: Search a keyword from other helm
  In order to search a keyword
  As a user
  I want to grep the keyword by helm-git-grep-from-helm

  Scenario: I grep a keyword by helm-git-grep-from-helm
    Given I start an action chain
    And I press "M-x"
    And I type "helm-for-files"
    And I press "RET"
    When I press "C-c g"
    And I type "upload_pack"
    When I press "RET"
    And I execute the action chain
    Then I should be in buffer matching regexp "git-clone.sh"
    Then the cursor should be at line "69"
