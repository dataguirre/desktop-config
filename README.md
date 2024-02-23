# Configuración PopOS!
## 1. Network drivers (UPDATE)
Hacer la antigua configuración (siguiente texto) deja el wifi mal. Para instalar los drivers correspondientes, 
```
sudo apt ubuntu-driver autoinstall
```
Esto instalara el otro driver (no b43-installer, sino el kernel...) que corre mucho mejor (en mi caso)
Otra configuración que puede boostear el wifi es
```
sudo nano /etc/NetworkManager/conf.d/default-wifi-powersave-on.conf
```
y poner el powersave mode en 2

## 1. Network drivers (DEPRECATED)
Si el wifi no está sirviendo, hay que instalar los drivers correspondientes. Primero, debemos obtener el PCI ID,
```
lspci -nn -d 14e4:
```
En nuestro portatil dio 14e4:4331, debemos instalar (hay que estar conectado a internet via cable ethernet) el paquete correspondiente (para 14e4:4331 es firmware-b43-installer). Para más información con la tabla del PCI ID al paquete de instalación correspondiente, revisar <a href="https://askubuntu.com/questions/55868/installing-broadcom-wireless-drivers" >este blog</a>
```bash
sudo apt update && sudo update-pciids
sudo apt install firmware-b43-installer && sudo apt install linux-firmware
sudo reboot
```
## 2. Cambiar wallpaper y foto de perfil
Ir al drive y descargar pirate vito y batalla de la cima (LOTR)

## 3. Instalar gnome extensions
Ir a https://extensions.gnome.org e instalar la extension. Descargar vitals y agregarla temperatura, memoria usada (en GB) y uso de procesador.
- Ir a extensions, pop shell setttings y eliminar el titulo de ventana.

## 4. Instalar gnome tweaks
```bash
sudo apt install gnome-tweaks -y
```
- Hacer swap de Ctrl y Bloq Mayus.

## 5. Instalar emacs.

```bash
sudo apt install emacs
```
## 6. Descargar Vivaldi
Ir a vivaldi descargas y descargar el archivo .deb
instalar en archivo:
```bash
sudo apt install /path/to/vivaldi-stable.deb
```
Ingresar y crear los perfiles correspondientes (Personal y Trabajo)

## 7. Instalar miniconda
Seguir los pasos de la web para instalar miniconda

## 8. Actualizar vista terminal
Para mostrar el último directorio y no todo el path y el git:
```bash
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\[\033[01;33m\]$(__git_ps1 " (%s)")\[\033[00m\]\$ '
```

