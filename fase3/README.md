# Como ejecutar los test

1. Moverse a la carpeta deseada y hacer el setup de meson
```bash
cd archivo-basico
meson setup build #Solo la primera vez
```

2. Usar la gramática dentro de la carpeta para generar el parser. Es el archivo `grammar.pegf90`

3. Mover el parser que se desea probar a la carpeta `src/`. Debe llamarse `parser.f90`
```bash
mv <ruta_del_parser>/parser.f90 <ruta_del_proyecto_con_los_tests>/src/parser.f90
```

4. Compilar y ejecutar los test
```bash
meson compile -C build

./build/tester # o tester.exe en Windows
```
