<?php

$home_dir = getenv("HOME");
$composer_bin =
    $home_dir . DIRECTORY_SEPARATOR . ".config/composer/vendor/autoload.php";

return [
    "commands" => [new \Psy\Command\ParseCommand()],
    "defaultIncludes" => [$composer_bin],
    "prompt" => "🐊",
    "startupMessage" => "Vai Corinthians!",
];
