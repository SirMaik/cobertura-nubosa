# **cobertura-nubosa**

## Descripción general

Se considera al **CCI** (índice de cobertura nubosa)  como el cociente del número de pixeles que se clasifican como nube sobre el número total de pixeles en el área de interés de una imágen. El objetivo de este proyecto es calcular el **CCI** de una imágen a partir de ésta y una máscara que delimite el área de la imágen que corresponde al cielo. 

## Instalación

En el caso de que no esté instalado es necesario instalar **stack**:

`$ wget -qO- https://get.haskellstack.org/ | sh`

Se pueden consultar otros modos de instalación en [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

Es necesario tener **stack** actualizado:

`$ stack upgrade`



Para construir el paquete es necesario pararse dentro del directorio *cobertura-nubosa* y ejecutar:
```
$ stack init
$ stack build
```
Y por último hay que copiar los binarios a una ruta conocida por el sistema:

`$ stack install`

*Nota: Al ejecutar este último comando se mostrará en pantalla la ruta en la que se guardó el binario por si se le quiere eliminar posteriormente.*

## Modo de uso 

Ahora es posible ejecutar el programa haciendo uso del comando `cobertura-nubosa-exe` junto con las banderas:

* `-h`  -  Muestra la versión y sección de ayuda.
* `-s`  -  Ruta a la imagen de entrada. **Esta bandera es obligatoria.**
* `-S`  -  En el caso de que se desee generar una imágen que muestra el cielo segmentado. Debe ir acompañada de:
  * `-o`  -  La ruta y nombre (con terminación *.png*) donde se guardará la imagen generada. 
* `-m`  -  En el caso de que se desee usar una máscara<sup>[\*]</sup> diferente a la predeterminada.
	
<sup>[\*]</sup> La máscara debe de ser una imágen de tamaño igual o estrictamente menor a la imágen de entrada. Se ignorarán todos los pixeles con canal alfa igual a cero  y se procesarán el resto. 

### Ejemplos

` $ cobertura-nubosa-exe -s images/11833.JPG`

Calculará el **CCI** de la imágen *images/11833.JPG*.

` $ cobertura-nubosa-exe -s images/11833.JPG -m data/masks/tight_mask.png -S -o 11833-seg.png`

Calculará el **CCI** de la imágen *images/11833.JPG* haciendo uso de la máscara *data/masks/tight_mask.png* y guardará la imagen generada en *11833-seg.png*. 


## Construído con
* [Stack](https://docs.haskellstack.org) - Programa multi-plataforma para desarrollar projectos en Haskell. 
* [Juicy Pixels](https://github.com/Twinside/Juicy.Pixels) - Biblioteca para cargar/guardar imágenes.
* [HFlags](https://github.com/nilcons/hflags) - Analizador sintáctico de banderas para Haskell conceptualmente similar a *gflags*x. 
