<?php

/**
 * Return a themed breadcrumb trail.
 *
 * @param $breadcrumb
 *   An array containing the breadcrumb links.
 * @return a string containing the breadcrumb output.
 */
function msgmobile_breadcrumb($variables) {
  $breadcrumb = $variables['breadcrumb'];

  if (!empty($breadcrumb)) {
    // Provide a navigational heading to give context for breadcrumb links to
    // screen-reader users. Make the heading invisible with .element-invisible.
    $output = '<h2 class="element-invisible">' . t('You are here') . '</h2>';

    $output .= '<div class="breadcrumb">' . implode(' › ', $breadcrumb) . '</div>';
    return $output;
  }
}

/**
 * Override or insert variables into the maintenance page template.
 */
function msgmobile_preprocess_maintenance_page(&$vars) {
  // While markup for normal pages is split into page.tpl.php and html.tpl.php,
  // the markup for the maintenance page is all in the single
  // maintenance-page.tpl.php template. So, to have what's done in
  // msgmobile_preprocess_html() also happen on the maintenance page, it has to be
  // called here.
  msgmobile_preprocess_html($vars);
}

/**
 * Override or insert variables into the html template.
 */
function msgmobile_preprocess_html(&$vars) {
  // Toggle fixed or fluid width.
  if (theme_get_setting('msgmobile_width') == 'fluid') {
    $vars['classes_array'][] = 'fluid-width';
  }
}

/**
 * Override or insert variables into the html template.
 */
function msgmobile_process_html(&$vars) {
  // Hook into color.module
  if (module_exists('color')) {
    _color_html_alter($vars);
  }
}

/**
 * Override or insert variables into the page template.
 */
function msgmobile_preprocess_page(&$vars) {
  // Move secondary tabs into a separate variable.
  $vars['tabs2'] = array(
    '#theme' => 'menu_local_tasks',
    '#secondary' => $vars['tabs']['#secondary'],
  );
  unset($vars['tabs']['#secondary']);

  if (isset($vars['main_menu'])) {
    $vars['primary_nav'] = theme('links__system_main_menu', array(
      'links' => $vars['main_menu'],
      'attributes' => array(
        'class' => array('links', 'inline', 'main-menu'),
      ),
      'heading' => array(
        'text' => t('Main menu'),
        'level' => 'h2',
        'class' => array('element-invisible'),
      )
    ));
  }
  else {
    $vars['primary_nav'] = FALSE;
  }
  if (isset($vars['secondary_menu'])) {
    $vars['secondary_nav'] = theme('links__system_secondary_menu', array(
      'links' => $vars['secondary_menu'],
      'attributes' => array(
        'class' => array('links', 'inline', 'secondary-menu'),
      ),
      'heading' => array(
        'text' => t('Secondary menu'),
        'level' => 'h2',
        'class' => array('element-invisible'),
      )
    ));
  }
  else {
    $vars['secondary_nav'] = FALSE;
  }

  // Prepare header.
  $site_fields = array();
  if (!empty($vars['site_name'])) {
    $site_fields[] = $vars['site_name'];
  }
  if (!empty($vars['site_slogan'])) {
    $site_fields[] = $vars['site_slogan'];
  }
  $vars['site_title'] = implode(' ', $site_fields);
  if (!empty($site_fields)) {
    $site_fields[0] = '<span>' . $site_fields[0] . '</span>';
  }
  $vars['site_html'] = implode(' ', $site_fields);

  // Set a variable for the site name title and logo alt attributes text.
  $slogan_text = $vars['site_slogan'];
  $site_name_text = $vars['site_name'];
  $vars['site_name_and_slogan'] = $site_name_text . ' ' . $slogan_text;

  $vars['noscript'] = t('This page requires JavaScript to be enabled, in order to work properly. ' .
    'Please, enable this option in your browser.');

  $static_url = msgmobile_get_static_url($base_url);
  $vars['theme_url'] = $static_url . '/' . path_to_theme();
  $vars['logo'] = $vars['theme_url'] . '/logo.png';
}

/**
 * Override or insert variables into the node template.
 */
function msgmobile_preprocess_node(&$vars) {
  $vars['submitted'] = $vars['date'] . ' — ' . $vars['name'];
}

/**
 * Override or insert variables into the comment template.
 */
function msgmobile_preprocess_comment(&$vars) {
  $vars['submitted'] = $vars['created'] . ' — ' . $vars['author'];
}

/**
 * Override or insert variables into the block template.
 */
function msgmobile_preprocess_block(&$vars) {
  $vars['title_attributes_array']['class'][] = 'title';
  $vars['classes_array'][] = 'clearfix';
}

/**
 * Override or insert variables into the page template.
 */
function msgmobile_process_page(&$vars) {
  // Hook into color.module
  if (module_exists('color')) {
    _color_page_alter($vars);
  }
}

/**
 * Override or insert variables into the region template.
 */
function msgmobile_preprocess_region(&$vars) {
  if ($vars['region'] == 'header') {
    $vars['classes_array'][] = 'clearfix';
  }
}

/**
 * Alter the HTML head tag.
 */
function msgmobile_html_head_alter(&$head_elements) {
  unset($head_elements['system_meta_generator']);
}

/**
 * Process theme variables.
 */
function msgmobile_process(&$vars) {

  global $base_url;

  $static_url = msgmobile_get_static_url($base_url);

  if (isset($vars['head'])) {
    $vars['head'] = str_replace($base_url, $static_url, $vars['head']);
  }

  if (isset($vars['page_top'])) {
    $vars['page_top'] = str_replace($base_url, $static_url, $vars['page_top']);
  }

  if (isset($vars['styles'])) {
    $vars['styles'] = str_replace($base_url, $static_url, $vars['styles']);
  }

  if (isset($vars['scripts'])) {
    $vars['scripts'] = str_replace($base_url, $static_url, $vars['scripts']);
  }

  if (isset($vars['page_bottom'])) {
    $vars['page_bottom'] = str_replace($base_url, $static_url, $vars['page_bottom']);
  }
}

/**
 * Returns the static domain for images, js, css, etc.
 */
function msgmobile_get_static_url($base_url) {

  //TODO: a certificate is needed for domain: static.mysmartgrid.de
  return ''; //str_replace('www', 'static', $base_url);
}

