# MetaCall Distributable Linux

Linux redistributable for shipping MetaCall Core.

## How to Generate the Distributable Tarballs

This will generate all tarballs in the `out` directory. All logs will be stored in `dist.log`. This process will test if tarballs are correct too.

```bash
make &> dist.log & tail -f dist.log
```

## How to Embed MetaCall as a Library from Distributable

In order to use MetaCall as a library in your applications, you can easily link `libmetacall.so` from the folder `/gnu/lib`. In order to set up additional environment variables required by the runtimes or MetaCall itself, run the following command before your application:

```sh
source /gnu/etc/profile
```

## How to Get the Hash from a Package

Replace `$URL` by the URL of the source code tarball, for example: `https://registry.npmjs.org/typescript/-/typescript-3.9.7.tgz`. Then paste the resulting hash into the package definition in the `.scm` file.

```bash
docker run --rm --privileged -it metacall/guix guix download $URL | tail -n1
```

In order to update hash from MetaCall Core package, just run the following command. It will automatically patch the hash in the `source/metacall.scm` file.

```bash
make download
```

## GitLab CI settings
Make sure to increase the job timeout to 2h+ (build job takes about a litle over an hour) in order to publish the tarball on GitHub as auto release you need to define the following variables in GitLab CI/CD settings, variable submenu:

* `GH_TOKEN` - a GitHub access token (select repo scope)
* `GH_REPO` - a GitHub repo in the format of **OWNER/REPO** e.g. `metacall/distributable-linux`
