<?php

/**
 * @file
 * Default theme implementation to display a forum which may contain forum
 * containers as well as forum topics.
 *
 * Variables available:
 * - $forums: The forums to display (as processed by forum-list.tpl.php)
 * - $topics: The topics to display (as processed by forum-topic-list.tpl.php)
 * - $forums_defined: A flag to indicate that the forums are configured.
 *
 * @see template_preprocess_forums()
 * @see theme_forums()
 */
?>
<?php global $user; ?>
<?php if ($forums_defined): ?>
<div id="forum">
  <?php if ($user->uid): ?>
  <p><?php print l(t('Create Forum Topic'), 'node/add/forum/0'); ?></p>
  <?php endif; ?>
  <?php print $forums; ?>
  <?php print $topics; ?>
</div>
<?php endif; ?>
