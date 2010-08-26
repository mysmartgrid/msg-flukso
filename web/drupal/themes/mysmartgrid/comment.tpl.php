<?php
// $Id: comment.tpl.php,v 1.5 2008/11/23 22:16:19 shannonlucas Exp $

$comment_class = 'comment' . (($comment->new) ? ' comment-new' : '') . 
                 ' ' . $status . ' ' . $zebra;
?>
<br>
<div class="<?php print $comment_class; ?>">

  <div class="comment-author">
    <span class="timestamp"><?php print format_date($comment->timestamp, 'custom', t('d M Y')); ?></span>
    <span class="node-author"> – <?php print t('Posted by'); ?> <?php print $author; ?></span>
  </div>

  <div class="comment-content">
    <?php if (!empty($picture)) { print $picture; } ?>
    <?php if ($comment->new): ?>
      <span class="new"><?php print $new ?></span>
    <?php endif; ?>
    <?php print $content ?>
    <?php if ($signature): ?>
      <div class="user-signature">
        <?php print $signature ?>
      </div>
    <?php endif; ?>
  </div>

  <div class="comment-links">
    <?php if ($links) { print $links; } ?>
  </div>
</div>
