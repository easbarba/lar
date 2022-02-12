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

  Valide o conteudo da ISO:
  
  sha256sum
  `cat SHA256SUMS | grep $(sha256sum debian-live-11.2.0-amd64-gnome+nonfree.iso | cut -f 1 -d " ")`

  sha512sum
  `cat SHA512SUMS | grep $(sha512sum debian-live-11.2.0-amd64-gnome+nonfree.iso | cut -f 1 -d " ")`

  Uma vez seguros da veracidade da nossa imagem, podemos então queimar a imagem no pendrive, e ha vários métodos para esse fim, eu prefiro o mais antigos e simples, `dd`:
  
  `sudo dd bs=4M if=/home/usuario/debian.iso of=/dev/sda status=progress oflag=sync`

  Para achar em qual dispositivo seu pendrive esta, use `df -h`, o meu esta em `/dev/sda`:
  
  `/dev/sda1       3,3G  3,3G     0 100% /media/easbarba/d-live nf 11.2.0 gn amd64`

  Lembre-se de apontar para o dispositivo principal `/dev/sda`, não seu sub `/dev/sda1`.

## Carregando Imagem Live
  Uma vez copiada no pendrive reinicie seu sistema, pare o carregamento e selecione o pendrive na lista de dispositivos achados pela sua placa-mãe.
  
  Uma vez carregada, selecione a primeira opção da lista: `Debian Live GNOME...`

  PS: em laptops Lenovo se para o carregamento com F12, mas em modelos antigos podem ser DELETE, ENTER ou ESC, para ter certeza consulte o manual do laptop ou placa-mãe.

## Licença
  [GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html)

