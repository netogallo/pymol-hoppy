from setuptools import Extension, setup

module1 = Extension(
    'pymol_hoppy_python',
    sources = ['pymol-hoppy-python/PymolHoppyPython.cpp'],
    include_dirs = [
        "dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-extra-0.1.0.0/build/",
        "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/include/",
        "/home/neto/code/pymol-open-source/layer0",
        "/home/neto/code/pymol-open-source/layer1",
        "/home/neto/code/pymol-open-source/layer2",
        "/home/neto/code/pymol-open-source/layer3",
        "/home/neto/code/pymol-open-source/layer4",
        "/home/neto/code/pymol-open-source/layer5",
        "/home/neto/code/pymol-open-source/ov/src",
        "/home/neto/code/pymol-open-source/include/"
    ],
    libraries = [
        'HSrts-ghc8.10.7',
        'HSbase-4.14.3.0-ghc8.10.7',
        "HSpymol-hoppy-0.1.0.0-inplace-ghc8.10.7",
        "HSpymol-hoppy-cpp-0.1.0.0-inplace-ghc8.10.7",
        "HSpymol-hoppy-extra-0.1.0.0-inplace-ghc8.10.7",
        "stdc++",
        # 'HScatamorphile-0.1.0.0-inplace-ghc8.10.7',
        # 'HScatamorphile-cpp-0.1.0.0-inplace-ghc8.10.7',
        # 'catamorphile',
        # '_cmd.cpython-310-x86_64-linux-gnu.so'
        'pymol'
    ],
    runtime_library_dirs = [
        #'/home/neto/code/pymol-open-source/build/lib.linux-x86_64-3.10/pymol/'
    ],
    library_dirs = [
        "dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-extra-0.1.0.0/build/",
        "dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-cpp-0.1.0.0/build/",
        "dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-0.1.0.0/build/",
        '/usr/local/lib',
        '/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts/',
        '/home/neto/code/pymol-open-source/build/lib.linux-x86_64-3.10/pymol/',
        '/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/base-4.14.3.0/',
        './'
    ])

setup (
    name = 'Pymol Hoppy Python',
    version = '1.0',
    description = 'Protein algoritms',
    ext_modules = [module1])

"""
    "HScatamorphine-0.1.0.0-inplace-ghc8.10.7",
    "HSinteger-gmp-1.1-ghc8.10.7",
    "HSghc-prim-0.8.0-ghc8.10.7",
    "HSbase-4.16.4.0-ghc8.10.7",
    "HStransformers-0.5.6.2-ghc8.10.7",
    "HSrts_l-ghc8.10.7"
    "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/base-4.16.4.0/",
    "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/ghc-prim-0.8.0/",
    "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/integer-gmp-1.1/",
    "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/transformers-0.5.6.2/",
    "/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts/"
    export LD_LIBRARY_PATH="/home/neto/code/pymol-open-source/contrib/catamorphine/dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-0.1.0.0/build/:/home/neto/code/pymol-open-source/contrib/catamorphine/dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-cpp-0.1.0.0/build:/home/neto/code/pymol-open-source/contrib/catamorphine/dist-newstyle/build/x86_64-linux/ghc-8.10.7/pymol-hoppy-extra-0.1.0.0/build:/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts/:/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/base-4.14.3.0/:/home/neto/code/pymol-open-source/contrib/catamorphine/"
"""
