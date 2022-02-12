# Debian GNU/Linux amd64 - Manual: Instalação Debian SID via debootstrap

Esse manual cobre os seguintes principais pontos:

- SID
- UEFI / GRUB
- WIFI
- Bluetooth
- LUKS
- BTRFS

# Objetivo
  Instalação de uma distro Debian pinada nos repositórios SID, que contem os pacotes mais atuais, via debootstrap, garante total flexibilidade.
  
  Esse distro também vai ter os principais discos encriptados via LUKS, com palavra-chave, e partições BTRFS para aproveitar 
  
  E claro conexão sem-cabo, WIFI.

## USB-stick 
  Para fácil instalação eu prefiro usar uma imagem ISO com GNOME, mas você pode escolher sua preferida aqui:
  
  https://cdimage.debian.org/cdimage/unofficial/non-free/cd-including-firmware/current-live/amd64/iso-hybrid/

  Ha vários métodos para queimar imagens ISO no seu pendrive eu prefiro o mais antigos e simples, `dd`:
  
  `sudo dd bs=4M if=/home/usuario/debian.iso of=/dev/sda status=progress oflag=sync`

  Para achar onde seu pendrive esta, use `df -h`, o meu esta em:
  
  `/dev/sda1       3,3G  3,3G     0 100% /media/easbarba/d-live nf 11.2.0 gn amd64`

  PS: Lembre de apontar para a `/dev/sda` e não `/dev/sda1`.

## Carregando Imagem Live
  Uma vez instalado a imagem reinicie o sistema, pare o carregamento e selecione o pendrive na lista de dispositivos achados pelo boot.
  
  Uma vez carregado selecione a primeira opção 'Debian Live *******'

  PS: em laptops Lenovo se para o carregamento com F12, mas em modelos antigos DELETE ou ENTER, ou procure qual o seu fabricante usa para parar o carregamento.


## license
  [GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html)

