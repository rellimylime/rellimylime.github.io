import sys, traceback

try:
    import zipfile
    import numpy as np
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from pathlib import Path
    from sklearn.decomposition import PCA, NMF
    from scipy.optimize import nnls
    print("imports OK")

    DATA_DIR = Path("data/HYDICE_Urban/")
    FIG_DIR = Path("figures/")
    FIG_DIR.mkdir(exist_ok=True)
    ZIP_PATH = DATA_DIR / "Urban.zip"

    with zipfile.ZipFile(ZIP_PATH) as zf:
        wvl_name = next(n for n in zf.namelist() if n.endswith(".wvl"))
        with zf.open(wvl_name) as f:
            lines = f.read().decode("ascii").replace("\r", "\n").splitlines()
            wavelengths = np.array([float(l.split("\t")[1]) for l in lines if l.strip()])

    with zipfile.ZipFile(ZIP_PATH) as zf:
        data_name = next(n for n in zf.namelist() if not n.endswith(("/", ".hdr", ".wvl")))
        with zf.open(data_name) as f:
            raw = np.frombuffer(f.read(), dtype=np.dtype(">i2"))

    H, W, B = 307, 307, 210
    cube = raw.reshape(H, B, W).transpose(1, 0, 2)
    print("load OK", cube.shape)

    def nearest_band(wl, nm): return int(np.argmin(np.abs(wl - nm)))
    R, G, Bi = nearest_band(wavelengths, 650), nearest_band(wavelengths, 550), nearest_band(wavelengths, 470)
    rgb = cube[[R, G, Bi]].astype(np.float32).transpose(1, 2, 0)
    lo, hi = np.percentile(rgb, [2, 98])
    rgb = np.clip((rgb - lo) / (hi - lo), 0, 1)
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.imshow(rgb)
    ax.set_title("HYDICE Urban — RGB composite", fontsize=11)
    ax.axis("off")
    plt.tight_layout()
    plt.savefig(FIG_DIR / "rgb_composite.png", bbox_inches="tight")
    plt.close()
    print("rgb_composite OK")

except Exception:
    traceback.print_exc()
    sys.exit(1)
