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
Una forma de instalarlo es directamente con apt, pero este no dará siempre la última actualización de emacs.

```bash
sudo apt install emacs
```
Para obtener las últimas versiones, se debe descargar emacs de la fuente. A continuación se encuentra una guía de GNU: https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html

Posibles errores: 
1. Puede ser necesario descargar las dependencias. ```sudo apt-get build-dep emacs```
2. Revisar una guía detallada para instalar emacs, seguir la guía de GNU y la siguiente: https://www.rahuljuliato.com/posts/compiling_emacs_29_2 

NOTA: no borrar la instalación (la carpeta con emacs-<VERSION>) para desinstalarlo en el futuro es necesario.
Antes de ejecutar el ```init.el```, revisar las dependencias del paquete vterm (Cmake y demás), necesarios para que se ejecute correctamente todo ```init.el```

Instalar fuentes: sudo apt install fonts-firacode fonts-cantarell
Instalar para `org-latex`: sudo apt install dvisvgm
Para instalar `pdf-tools`, eliminar la parte que hice :pin manual y cerrar y abrir emacs otra vez, va a instalar varios paquetes (como libgcc) y ya volverlo a poner.
## 6. Descargar Vivaldi
Ir a vivaldi descargas y descargar el archivo .deb
instalar en archivo:
```bash
sudo apt install /path/to/vivaldi-stable.deb
```
Ingresar y crear los perfiles correspondientes (Personal y Trabajo)

## 7. Instalar miniconda (DEPRECATED)
Seguir los pasos de la web para instalar miniconda

## 7. Instalar UV
UV es un manejador de paquetes de python más rápido y eficiente que conda. 

## 8. Actualizar vista terminal
Para mostrar el último directorio y no todo el path y el git:
```bash
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\[\033[01;33m\]$(__git_ps1 " (%s)")\[\033[00m\]\$ '
```
## 9. Conectar VPN por comando
Agregar el siguiente código en el archivo ```.bashrc```. Se está usando open connect, este tiene distintos protocolos que permite conectarse a VPNs, en algunos casos como Cisco (la de la universidad), toca descargar su herramienta.
```bash
connect-vpn() {
    case $1 in
        <VPN1>)
            echo "<PASSWORD_VPN1>" | sudo openconnect \
                --user=<USER_VPN1> \
                --passwd-on-stdin \
                --protocol=fortinet \
                200.26.145.18:10443 \
                --servercert pin-sha256:wo/guFvTHsF8G/3iV8dIy276hO/b2TadvIz7nfJQ7js=
            ;;
        <VPN2>)
            echo "<PASSWORD_VPN2>" | sudo openconnect \
                --user=<USER_VPN2> \
                --passwd-on-stdin \
                --protocol=fortinet \
                your-ecp-server-address \
                --servercert your-ecp-server-cert
            ;;
        *)
            echo "Usage: connect-vpn {<VPN1>|<VPN2>}"
            ;;
    esac
}

```
## 10. Conectarse a servidores por SSH.
Agregar en el archivo ```.ssh/config``` la siguiente información de los servidores:

```
Host <NOMBRE SERVIDOR>
HostName <DIRRECION IP>
User <USUARIO>
```

Usar el comando ```ssh <NOMBRE SERVIDOR>``` para conectarse al servidor a través del archivo generado. De lo contrario, ```ssh <USUARIO>@<IP SERVIDOR>```.

Para no ingresar cada vez la contraseña, se debe inscribir el computador en el servidor. Para esto, se genera una llave SSH en el computador del que se accede y se inscribe en la máquina remota. 
1. Generar la llave pública:
```bash
ssh-keygen
```
2. Copiar la llave en la máquina remota:
```bash
ssh-copy-id remote_username@remote_server_ip_address
```



