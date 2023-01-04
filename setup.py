from setuptools import Extension, setup

module1 = Extension(
    'catamorphile',
    sources = ['catamorphile-python/catamorphile.cpp'],
    include_dirs = [
        'dist-newstyle/build/x86_64-linux/ghc-8.10.7/catamorphile-0.1.0.0/build/',
        '/home/neto/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/include/'
    ],
    libraries = [
        'HSrts-ghc9.2.5',
        'HScatamorphile-0.1.0.0-inplace-ghc8.10.7',
        'HScatamorphile-cpp-0.1.0.0-inplace-ghc8.10.7',
        'catamorphile',
        # '_cmd.cpython-310-x86_64-linux-gnu.so'
        'pymol'
    ],
    runtime_library_dirs = [
        #'/home/neto/code/pymol-open-source/build/lib.linux-x86_64-3.10/pymol/'
    ],
    library_dirs = [
        'dist-newstyle/build/x86_64-linux/ghc-8.10.7/catamorphile-0.1.0.0/build/',
        'dist-newstyle/build/x86_64-linux/ghc-8.10.7/catamorphile-cpp-0.1.0.0/build',
        '/usr/local/lib',
        '/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/rts/',
        # '/home/neto/code/pymol-open-source/build/lib.linux-x86_64-3.10/pymol/'
        './'
    ])

setup (
    name = 'Catamorphile',
    version = '1.0',
    description = 'Protein algoritms',
    ext_modules = [module1])

"""
    "HScatamorphine-0.1.0.0-inplace-ghc9.2.5",
    "HSinteger-gmp-1.1-ghc9.2.5",
    "HSghc-prim-0.8.0-ghc9.2.5",
    "HSbase-4.16.4.0-ghc9.2.5",
    "HStransformers-0.5.6.2-ghc9.2.5",
    "HSrts_l-ghc9.2.5"
    "/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/base-4.16.4.0/",
    "/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/ghc-prim-0.8.0/",
    "/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/integer-gmp-1.1/",
    "/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/transformers-0.5.6.2/",
    "/home/neto/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/rts/"
"""
