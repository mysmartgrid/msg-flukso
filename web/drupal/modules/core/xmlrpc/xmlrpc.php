<?php

/**
 * @file
 * PHP page for handling incoming XML-RPC requests from clients.
 */

// define xmlrpc method location
define('XMLRPC_PATH',     'sites/all/modules');
define('XMLRPC_MODULE',   'logger');
define('XMLRPC_FILE',     'xmlrpc');

// defined xmlrpc endpoints
$xmlrpc_versions = array('', 1);

// any common.inc or other core functions that xmlrpc processing relies upon
function t($string, $args = array(), $langcode = NULL) {
  if (empty($args)) {
    return $string;
  }
  else {
    // Transform arguments before inserting them.
    foreach ($args as $key => $value) {
      switch ($key[0]) {
        case '@':
        case '%':
        default:
          // Escaped only.
          $args[$key] = check_plain($value);
          break;

        case '!':
          // Pass-through.
      }
    }
    return strtr($string, $args);
  }
}

function watchdog_xmlrpc($type, $message, $variables = array(), $severity = WATCHDOG_NOTICE, $link = NULL) {
  global $base_root;

  $current_db = db_set_active();

  db_query("INSERT INTO {watchdog}
    (type, message, variables, severity, link, location, referer, hostname, timestamp)
    VALUES
    ('%s', '%s', '%s', %d, '%s', '%s', '%s', '%s', %d)",
    $type,
    $message,
    serialize($variables),
    $severity,
    $link,
    $base_root . request_uri(),
    referer_uri(),
    ip_address(),
    time());

  if ($current_db) {
    db_set_active($current_db);
  }
}

include_once './includes/bootstrap.inc';
drupal_bootstrap(DRUPAL_BOOTSTRAP_DATABASE);//DRUPAL_BOOTSTRAP_FULL
include_once './includes/xmlrpc.inc';
include_once './includes/xmlrpcs.inc';

//xmlrpc_server(module_invoke_all('xmlrpc'));

if (in_array($_REQUEST['version'], $xmlrpc_versions)) {
  require_once XMLRPC_PATH . '/' . XMLRPC_MODULE . '/' . XMLRPC_FILE . $_REQUEST['version'] . '.inc';

  //debugging watchdog_xmlrpc('xmlrpc', 'xmlrpc api called with version %version', array('%version' => $_REQUEST['version']), WATCHDOG_DEBUG);

  $function  = XMLRPC_MODULE . '_xmlrpc';
  $callbacks = $function();
  xmlrpc_server($callbacks);
}
else {
  xmlrpc_server_error(-32601, t('Server error. Requested method version (@version) not specified.', array("@version" => $_REQUEST['version'])));
}
