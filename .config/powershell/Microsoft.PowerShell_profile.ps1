# ----------
# Enviornment Variables

$env:DefaultUser = $env:USER # hide domain in SSH tunnels
if ($IsLinux) { $env:DATA = "/dados/" } else { $env:DATA = "D:\" }
$env:CERO = Join-Path $env:DATA  "Pessoal" "Cero"

# ----------
# Alias

# Set-Alias name cmd

# ----------
# Functions

function ceroit { cd $env:CERO; sh build.sh }
function cerod  { cero distro @args }
function cerodi { cero distro install @args }
function cerodu { cero distro update; cero distro upgrade }
function cerodr { cero distro remove @args }
function cerods { cero distro search @args }

# ----------
# Modules

$modules = "posh-git", "InvokeBuild", "oh-my-posh"
function lxModulesInstall {
    foreach ($module in $modules){
	if (!(Get-Module -ListAvailable -Name $module)){ Install-Module $module -Scope CurrentUser -Force }
    }
}

# Importing modules
foreach ($module in $modules){ Import-Module $module }

# Module Specific Settings
if (Get-Module -ListAvailable -Name oh-my-posh){ Set-Theme Sorin }

Invoke-Expression (&starship init powershell)

# ----------
# NuGet Packages
function lxNugetInstall {
    $nuGetPackages = "pwsh", "dotnet-script"
    foreach ($pack in $nuGetPackages){ if (!(Get-Command $pack)){ dotnet tool install --global $pack } }
}

# ---------------
# Chocolatey
function lxChocoIt {
    #  Install Choco
    if (!(Get-Command choco)) {
	Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
    }
}

$chocoPackages = '7zip', 'googlechrome', 'vlc', 'firefox', 'mpv', 'ffmpeg',
'gimp', 'python3', 'curl', 'calibre', 'git', 'emacs', 'nodejs', 'libreoffice-fresh', 'spotify', 'pandoc'
function lxChocoInstall {foreach ($pack in $chocoPackages){ choco install $pack }}
function lxChocoUpgrade {foreach ($pack in $chocoPackages){ choco upgrade $pack }}


# --------------------------------------------------------------
# Legacy


# TODO zplug-like setters
# function moduleSettings {
    #     Param($Module, $Setting)
    #     if (Get-Module -ListAvailable -Name $Module){ $Setting }
    # }

# moduleSettings -Module "oh-my-posh" -Setting "Set-Theme Sorin"

# "`nNew-Alias which get-command" | add-content $profile
