<?php
/**
 * @file template.php
 * Core functions for the Flukso theme.
 *
 * $Id$
 */


/**
 * Add a prefix to the terms list and insert a separateor between them.
 *
 * @param $terms The pre-rendered HTML string containing the term list
 *        elements.
 * @param $prefix The text to show before the list of terms. By default the
 *        output of t('Tags: ') is used.
 * @param $separator The character(s) to place between the terms. By default,
 * 		  the output of t(', ') is used.
 *
 * @return The modified HTML.
 */
function mysmartgrid_separate_terms($terms, $prefix = NULL, $separator = NULL) {
  $prefix    = ($prefix == NULL) ? t('Tags: ') : $prefix;
  $separator = ($separator == NULL) ? t(', ') : $separator;
  $output    = $prefix . preg_replace('!a></li>\s<li!',
                                      "a>{$separator}</li>\n<li", $terms);
  return $output;
}


/**
 * Insert a separator between items in the list of links for a node.
 *
 * @param $links The pre-rendered HTML string containing the link list
 *        elements.
 * @param $separator character(s) to place between the links. By default, the
 *        output of t(' | ') is used.
 *
 * @return The modified HTML.
 */
function mysmartgrid_separate_links($links, $separator = ' | ') {
  $separator = ($separator == NULL) ? t(' | ') : $separator;
  $output    = preg_replace('!a></li>\s<li!',
                            "a>{$separator}</li>\n<li", $links);
  return $output;
}


/**
 * Preprocess the nodes.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        these keys will be added to $vars:
 *        - 'mysmartgrid_node_author' - The node's "posted by" text and author
 *          link.
 *        - 'mysmartgrid_node_class' - The CSS classes to apply to the node.
 *        - 'mysmartgrid_node_links' - The node links with a separator placed
 *          between each.
 *        - 'mysmartgrid_perma_title' - The localized permalink text for the node.
 *        - 'mysmartgrid_node_timestamp' - The timestamp for this type, if one
 *          should be rendered for this type.
 *        - 'mysmartgrid_term_links' - The taxonomy links with a separator placed
 *          between each.
 */
function mysmartgrid_preprocess_node(&$vars) {
  $node                       = $vars['node'];
  $vars['mysmartgrid_node_class']  = 'node ' . ($node->sticky ? 'sticky ' : '') .
                                ($node->status ? '' : ' node-unpublished') .
                                ' node-' . $node->type .
                                ($teaser ? ' teaser' : '') . ' clear-block';
  $vars['mysmartgrid_term_links']  = mysmartgrid_separate_terms($vars['terms']);
  $vars['mysmartgrid_node_links']  = mysmartgrid_separate_links($vars['links']);
  $vars['mysmartgrid_perma_title'] = t('Permanent Link to !title',
                                array('!title' => $vars['title']));

  // --------------------------------------------------------------------------
  // -- Node authorship.
  if (!empty($vars['submitted'])) {
    $vars['mysmartgrid_node_author'] = t('Posted by !author',
                                    array('!author' => $vars['name']));
  }

  // --------------------------------------------------------------------------
  // -- Timestamp for this type?
  if (!empty($vars['submitted']) && isset($node->created)) {
    $vars['mysmartgrid_node_timestamp'] = format_date($node->created, 'custom', t('d M Y'));
  }
}

/**
 * Preprocess the pages.
 *
 * @param &$vars The template variables array. After invoking this function,
 *        no page title will be displayed on /node/x pages.
 */
function mysmartgrid_preprocess_page(&$vars) {
  if (substr($_GET['q'], 0, 4) == 'node') {
    $vars['title'] = ''; 
  }

 // -- tab text sould always be Flukso
 $vars['head_title'] = 'mySmartGrid';
}

/**
  * Support for image_annotate on image nodes
  *
  */
function phptemplate_image_body($node, $size) {
  if (user_access('view image annotations') || user_access('create image annotations') || user_access('administer image annotations')) {
    // Retrieve all the annotations for that image field
    // We sort by area (height*width) to make sure small annotations are always on the top and avoid having some unhoverable ones
    $result = db_query('SELECT i.*, c.uid, c.comment, u.name FROM {image_annotate} i INNER JOIN {comments} c ON i.cid = c.cid JOIN {users} u ON c.uid = u.uid WHERE c.nid = %d ORDER BY (i.size_height*i.size_width) ASC', $node->nid);

    // Build the array of notes settings
    global $user;
    $notes = array();
    while ($note = db_fetch_object($result)) {
      $editable = user_access('administer image annotations') || (user_access('create image annotations') && $note->uid && $note->uid == $user->uid);
      $author = theme('username', $note);
      $text = check_plain($note->comment); // . '"<span class="author"> '. t('by') .' '. $author . '</span>';

//      if (user_access('access comments')) {
//        $text .= '<span class="actions"> » '. l(t('View comment'), $_GET['q'], array('fragment'=>'comment-'. $note->cid)) .'</span>';
//      }

      $notes[] = array(
        'aid' => $note->aid,
        'cid' => $note->cid,
        'uid' => $note->uid,
        'height' => $note->size_height,
        'width' => $note->size_width,
        'top' => $note->position_top,
        'left' => $note->position_left,
        'text' => $text,
        'editable' => $editable,
      );
    }
   
    // Build the field settings
    $settings = array(array(
      'nid' => $node->nid,
      'field' => 'image',
      'notes' => $notes,
      'editable' => user_access('administer image annotations') || user_access('create image annotations'),
    ));
   
    module_load_include('module', 'jquery_ui');
    // Load all the JS and CSS magic
    drupal_add_js(array('imageAnnotate' => $settings), 'setting');
    jquery_ui_add(array('ui.resizable', 'ui.draggable'));
    drupal_add_js('misc/collapse.js');
    drupal_add_js(drupal_get_path('module', 'image_annotate') .'/tag.js');
    drupal_add_css(drupal_get_path('module', 'image_annotate') .'/tag.css');
    //BVDM 13/09/09: substitute image-annotate-image for image-annotate-nid-$node->nid to create a unique class per inserted image
    $class = 'imagefield imagefield-image image-annotate-nid-' . $node->nid;
    return image_display($node, $size, array('class' => $class));
  }
}

/**
  * Support for image_annotate on img_assist inserted images
  *
  */
function phptemplate_img_assist_inline($node, $size, $attributes) {
  $caption = '';
  if ($attributes['title'] && $attributes['desc']) {
    $caption = '<strong>'. $attributes['title'] .': </strong>'. $attributes['desc'];
  }
  elseif ($attributes['title']) {
    $caption = '<strong>'. $attributes['title'] .'</strong>';
  }
  elseif ($attributes['desc']) {
    $caption = $attributes['desc'];
  }
  // Change the node title because img_assist_display() uses the node title for
  // alt and title.
  $node->title = strip_tags($caption);

  // --------------------------

  if (user_access('view image annotations') || user_access('create image annotations') || user_access('administer image annotations')) {
    // Retrieve all the annotations for that image field
    // We sort by area (height*width) to make sure small annotations are always on the top and avoid having some unhoverable ones
    $result = db_query('SELECT i.*, c.uid, c.comment, u.name FROM {image_annotate} i INNER JOIN {comments} c ON i.cid = c.cid JOIN {users} u ON c.uid = u.uid WHERE c.nid = %d ORDER BY (i.size_height*i.size_width) ASC', $node->nid);

    // Build the array of notes settings
    global $user;
    $notes = array();
    while ($note = db_fetch_object($result)) {
      $editable = user_access('administer image annotations') || (user_access('create image annotations') && $note->uid && $note->uid == $user->uid);
      $author = theme('username', $note);
      $text = check_plain($note->comment); // . '"<span class="author"> '. t('by') .' '. $author . '</span>';

//      if (user_access('access comments')) {
//        $text .= '<span class="actions"> » '. l(t('View comment'), $_GET['q'], array('fragment'=>'comment-'. $note->cid)) .'</span>';
//      }

      $notes[] = array(
        'aid' => $note->aid,
        'cid' => $note->cid,
        'uid' => $note->uid,
        'height' => $note->size_height,
        'width' => $note->size_width,
        'top' => $note->position_top,
        'left' => $note->position_left,
        'text' => $text,
        // notes should be added when residing on the image page
        'editable' => FALSE,
      );
    }
   
    // Build the field settings
    $settings = array(array(
      'nid' => $node->nid,
      'field' => 'image',
      'notes' => $notes,
      'editable' => user_access('administer image annotations') || user_access('create image annotations'),
    ));

    module_load_include('module', 'jquery_ui');
    // Load all the JS and CSS magic
    drupal_add_js(array('imageAnnotate' => $settings), 'setting');
    jquery_ui_add(array('ui.resizable', 'ui.draggable'));
    drupal_add_js('misc/collapse.js');
    drupal_add_js(drupal_get_path('module', 'image_annotate') .'/tag.js');
    drupal_add_css(drupal_get_path('module', 'image_annotate') .'/tag.css');
    //BVDM 13/09/09: substitute image-annotate-image for image-annotate-nid-$node->nid to create a unique class per inserted image
    $class = 'imagefield imagefield-image image-annotate-nid-' . $node->nid;
    $img_tag = img_assist_display($node, $size, array('class' => $class));
  }
  else {
    $img_tag = img_assist_display($node, $size);
  }

  // -----------------------

  // Always define an alignment class, even if it is 'none'.
  $output = '<span class="inline inline-'. $attributes['align'] .'">';

  $link = $attributes['link'];
  $url  = '';
  // Backwards compatibility: Also parse link/url in the format link=url,foo.
  if (strpos($link, ',') !== FALSE) {
    list($link, $url) = explode(',', $link, 2);
  }
  elseif (isset($attributes['url'])) {
    $url = $attributes['url'];
  }
  
  if ($link == 'node') {
    $output .= l($img_tag, 'node/'. $node->nid, array('html' => TRUE));
  }
  elseif ($link == 'popup') {
    $popup_size = variable_get('img_assist_popup_label', IMAGE_PREVIEW);
    $info       = image_get_info(file_create_path($node->images[$popup_size]));
    $width      = $info['width'];
    $height     = $info['height'];
    $popup_url  = file_create_url($node->images[variable_get('img_assist_popup_label', IMAGE_PREVIEW)]);
    $output .= l($img_tag, $popup_url, array('attributes' => array('onclick' => "launch_popup({$node->nid}, {$width}, {$height}); return false;", 'target' => '_blank'), 'html' =>TRUE));
  }
  elseif ($link == 'url') {
    $output .= l($img_tag, $url, array('html' => TRUE));
  }
  else {
    $output .= $img_tag;
  }
  
  if ($caption) {
    if ($attributes['align'] != 'center') {
      $info = image_get_info(file_create_path($node->images[$size['key']]));
      // Reduce the caption width slightly so the variable width of the text
      // doesn't ever exceed image width.
      $width = $info['width'] - 2;
      $output .= '<span class="caption" style="width: '. $width .'px;">'. $caption .'</span>';
    }
    else {
      $output .= '<span class="caption">'. $caption .'</span>';
    }
  }
  $output .= '</span>';
  return $output;
}

////Added by Stephan Platz, 23.04.2010
///**
// * * Override or insert PHPTemplate variables into the search_theme_form template.
// * *
// * * @param $vars
// * *   A sequential array of variables to pass to the theme template.
// * * @param $hook
// * *   The name of the theme function being called (not used in this case.)
// * */
//function mysmartgrid_preprocess_search_theme_form(&$vars, $hook) {
//  // Note that in order to theme a search block you should rename this function
//  // to yourthemename_preprocess_search_block_form and use
//  // 'search_block_form' instead of 'search_theme_form' in the customizations
//  // bellow.
//  
//  // Modify elements of the search form
//  $vars['form']['search_theme_form']['#title'] = t('');
//  
//  // Set a default value for the search box
//  $vars['form']['search_theme_form']['#value'] = t('Search this Site');
//  
//  // Add a custom class to the search box
//  $vars['form']['search_theme_form']['#attributes'] = array('class' => 'NormalTextBox txtSearch',
//                                                            'onfocus' => "if (this.value == 'Search this Site') {this.value = '';}",
//                                                            'onblur' => "if (this.value == '') {this.value = 'Search this Site';}");
//  
//  // Change the text on the submit button
//  //$vars['form']['submit']['#value'] = t('Go');
//  
//  // Rebuild the rendered version (search form only, rest remains unchanged)
//  unset($vars['form']['search_theme_form']['#printed']);
//  $vars['search']['search_theme_form'] = drupal_render($vars['form']['search_theme_form']);
//  
//  $vars['form']['submit']['#type'] = 'image_button';
//  $vars['form']['submit']['#src'] = path_to_theme() . '/img/search-grey.gif';
//    
//  // Rebuild the rendered version (submit button, rest remains unchanged)
//  unset($vars['form']['submit']['#printed']);
//  $vars['search']['submit'] = drupal_render($vars['form']['submit']);
//  
//  // Collect all form elements to make it easier to print the whole form.
//  $vars['search_form'] = implode($vars['search']);
//}
