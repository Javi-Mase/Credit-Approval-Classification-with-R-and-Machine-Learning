@echo off
echo Agregando todos los cambios...
git add .

echo Realizando commit con el mensaje proporcionado...
git commit -m "%1"

echo Enviando cambios a la rama 'main' en el repositorio remoto...
git push origin main

echo Operación completada.
pause
