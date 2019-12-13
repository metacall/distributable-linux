# MetaCall Distributable

Cross-platform and multi-arch redistributable for shipping MetaCall Core.

## How to Generate the Distributable Tarballs

This will generate all tarballs in the `out` directory. All logs will be stored in `dist.log`. This process will test if tarballs are correct too.

```bash
make &> dist.log & tail -f dist.log
```
