

# Guix configuration

This is my personal set of configuration files built around the [GNU Guix](https://guix.gnu.org/) package manager and the [RDE](https://git.sr.ht/~abcdw/rde) configuration framework.  

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><img src="https://files.migalmoreno.com/guix_config_light.jpg" alt="guix_config_light.jpg" /></td>
<td class="org-left"><img src="https://files.migalmoreno.com/guix_config_dark.jpg" alt="guix_config_dark.jpg" /></td>
</tr>
</tbody>
</table>

The main goal behind this repository is to hold the configurations of my users, hosts, and remote machines in a single dispatch entry point. For the most part, I stick to the RDE principles to choose my tooling and I try to contribute to and maintain related programs.  

-   **OS/Package Manager:** GNU Guix
-   **Window Manager:** dwl-guile
-   **Status Bar:** dtao-guile
-   **Menu Prompt:** bemenu + j4-dmenu-desktop
-   **Browser:** Nyxt
-   **Text Editor:** GNU Emacs
-   **Video Player:** mpv

To build and activate my configurations, invoke one of the following commands:  

    # To activate the current user's home
    make home
    # For a specific user
    make home/reconfigure/$USER
    # To activate the current hostname's config
    make system
    # For a specific host
    make system/reconfigure/$HOST
    # To deploy an entire configuration for a remote machine
    make deploy/$HOST/$USER

To alleviate maintenance burden, I prefer working on top of Guix community [channels](https://guix.gnu.org/manual/en/html_node/Channels.html) to implement new features, which I define through lock files under the `./rde/` directory. If you wish to reproduce one of my setups, please make sure to consult the pinned commits on these files.  

