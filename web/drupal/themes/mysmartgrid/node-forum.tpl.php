<?php
/**
 * @file node.tpl.php
 * The node rendering logic for Flukso.
 *
 * In addition to the standard variables Drupal makes available to node.tpl.php,
 * these variables are made available by the theme:
 *
 * - $mysmartgrid_node_author - The node's "posted by" text and author link.
 *
 * - $mysmartgrid_node_class - The CSS classes to apply to the node.
 *
 * - $mysmartgrid_node_links - The node links with a separator placed between each.
 *
 * - $mysmartgrid_perma_title - The localized permalink text for the node.
 *
 * - $mysmartgrid_term_links - The taxonomy links with a separator placed between
 *   each.
 *
 * - $mysmartgrid_node_timestamp - The timestamp for this type, if one should be
 *   rendered for this type.
 *
 * $Id$
 */
?>
<div id="node-<?php print $node->nid; ?>" class="<?php echo $mysmartgrid_node_class; ?>">

  <div class="node-headline clear-block">
    <span class="timestamp"><?php print $mysmartgrid_node_timestamp; ?></span>
    <span class="node-author"> – <?php print $mysmartgrid_node_author; ?></span>
    <br><br>
  </div>

  <div class="content clear-block">
    <?php print $picture; ?>
    <?php print $content; ?>
  </div>

  <?php if (!stripos(request_uri(), 'reply')): ?>
    <div class="meta">
        <div class="more"><?php print $mysmartgrid_node_links; ?></div>
    </div>
  <?php endif; ?>

</div> <!-- node -->
