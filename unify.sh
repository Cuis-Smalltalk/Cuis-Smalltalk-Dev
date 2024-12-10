create_unified_vm_macOS() {
    local MISMATCHINGNIBS=
    local MISMATCHINGPLISTS=

    readonly O="$1"
    readonly A="$2"
    readonly B="$3"

    if [ ! -d "$A" ]; then
    echo "$A does not exist; aborting"
    exit 44
    fi

    if [ ! -d "$B" ]; then
    echo "$B does not exist; aborting"
    exit 45
    fi

    echo "merging $A \& $B into $O..."
    mkdir -p $O

    for f in `cd $A >/dev/null; find . | sed 's|^\.\/||'`; do
    if [ -d "$A/$f" ]; then
        mkdir -p $O/$f
    # elif [ -L "$A/$f" ]; then
    #   echo ln -s `readlink "$A/$f"` "$O/$f"
    elif [ ! -f "$A/$f" ]; then
        echo  "$A/$f does not exist; how come?"
    elif [ ! -f "$B/$f" ]; then
        echo  "$B/$f does not exist; how come?"
    else
        case `file -b "$A/$f"` in
        Mach-O*)
            lipo -create -output "$O/$f" "$A/$f" "$B/$f";;
        *)
            if cmp -s "$A/$f" "$B/$f"; then
            cp "$A/$f" "$O/$f"
            else
            echo "EXCLUDING $f because it differs"
            case "$f" in
                *.plist)
                MISMATCHINGPLISTS="$MISMATCHINGPLISTS $f"
                ;;
                *.nib)
                MISMATCHINGNIBS="$MISMATCHINGNIBS   $f"
                echo "using $B version"
                cp "$B/$f" "$O/$f"
                ;;
            esac
            fi
        esac
    fi
    done
}

create_unified_vm_macOS ./Unified ./ARM ./Intel
