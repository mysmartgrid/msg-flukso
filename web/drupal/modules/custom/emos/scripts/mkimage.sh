#!/bin/sh

template_file=
target_file=
ip=
netmask=
gateway=

for param in "$@"
do
	case $param in
		template=*)
			template_file=${param#template=}
			;;

		target=*)
			target_file=${param#target=}
			;;

		ip=*)
			ip=${param#ip=}
			;;

		netmask=*)
			netmask=${param#netmask=}
			;;

		gateway=*)
			gateway=${param#gateway=}
			;;
	esac
done

if [ "x$template_file" = "x" ]
then
	echo "No template given"
	return 1
fi
if [ "x$ip" = "x" ]
then
	echo "No IP given"
	return 1
fi
if [ "x$netmask" = "x" ]
then
	echo "No netmask given"
	return 1
fi
if [ "x$gateway" = "x" ]
then
	echo "No gateway given"
	return 1
fi

if [ "x$USER" != xroot ]
then
	echo "Must run as root"
	return 2
fi

if ! [ -f "$template_fil" ]
then
	echo "Template does not exist"
	return 3
fi

if [ "x$target_file" = "x" ]
then
	target_file="image_$ip.img"
fi

dir=`mktemp -d`

cp "$template_file" "$target_file"
mount -oloop,offset=$((122880*512)) "$target_file" "$dir"
sed -e "s/iface eth0 inet dhcp/iface eth0 inet static\n\taddress $ip\n\tnetmask $netmask\n\tgateway $gateway/" -i "$dir/etc/network/interfaces"
umount "$dir"
rmdir "$dir"
