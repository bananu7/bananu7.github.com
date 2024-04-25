---
title: Nova, a homegrown kubernetes - part 2
date: 2024-04-25
layout: post
categories:
- kubernetes
---

# Running a (micro)k8s cluster at home - part 2

I was supposed to write about the setup, but I got a bit sidetracked, so instead it's going to be a post about my home automation endeavors.

## Goal

I have an Ikea desk lamp with the switch on the cable, and always found it super annoying to turn off manually. I've decided to give myself an option to do so wirelessly.

While I could have simply bought a "smart passthrough socket", I wanted to avoid vendor lock-in, as well as prototype a bit more of interesting software.

## Home Assistant

The key component of the deployment is going to be Home Assistant - HA for short. Normally, it's deployed on an RPi using the dedicated
HA operating system, or in a VM. The standalone/container deployments are much less supported. Of course, I've decided to go the hard route and deploy it on nova.

I've used [this](https://github.com/pajikos/home-assistant-helm-chart) chart, and only had to provide basic value setup:

```
persistence:
  enabled: true
  storageClass: longhorn
```

Longhorn auto-provisioned the storage for me and after adding a simple ingress it was up and running.

## ESPHome

The second part of the equation was ESPHome, a (supposedly) better, more modern Tasmota alternative, for provisioning ESP devices as HA-compatible nodes. This was more of a bumpy ride. Normally, ESPHome is installed as a HA addon via a nice browser button. That option isn't available, if you use HA in a container, though.

Thankfully, ESPHome is a completely standalone service, merely used to build and flash the nodes, and the provisioning is then done manually. So,
I wrote a simple Deployment
```yaml

      containers:
        - name: esphome
          image: ghcr.io/esphome/esphome
          imagePullPolicy: IfNotPresent
          ports:
            - containerPort: 80
              name: http
          volumeMounts:
            - mountPath: "/cache"
              name: cache
            - mountPath: "/config"
              name: config
...
```

And two PVCs for longhorn, and it started right off. My initial sizes were much too small, though - I recommend 5Gi for cache and 500Mi for config.

Unfortunately, the bumpy ride wasn't yet over. Because I didn't serve it over https (because I still don't have full NAT hairpin and my letsencrypt
cert-manager fix was only for `home.banachewicz.pl` domain), i couldn't use WebUSB to connect to my board directly. Instead, I used a helpful
[hosted version](https://web.esphome.io/) to initially provision the node. At first I wanted to use my RISC-V C3 devkit, but that one failed in a bootloop, so I used another WROOM32 I had lying around. After the initial flash, the device connected to wifi, but I was still not seeing it in my 
dashboard. I tried flashing it by building the image then uploading it from the device web iface, but that seemingly bricked it. Thankfully, the 
ESPHome Discord server was super helpful in making me realize that a) I needed to enable a web server back:

```yaml
web_server:
  port: 80
```

And add the ip address of the device manually

```yaml
wifi:
  use_address: "..."
```

That second part was because I'm still not running HA's mDNS service which gives every device a nice name. This _still_ didn't make it appear in the dashboard, but I was nevertheless able to build and flash the firmware directly from it anyway. Oh well. Apparently there's a configuration option to switch the dashboard to ping scan instead.

## Practical use

At last, I could add an output to my node:

```yaml
light:
  - platform: status_led
    name: "Builtin LED"
    pin: GPIO2
```

connect it to HA, and it immediately showed up as a usable device!

![HA interface showing a toggled switch and an ESP board with its builtin LED turned on](/images/esphome_led.jpg)

I call that a win. The main reason to use ESPHome is to integrate with a lot of stuff very quickly. There's tons and tons of builtin libraries that turn proprietary interfaces into HA actions and sensors.

## Remarks

While this was a mild success, I'll now need to wire a power switch to my ESP board, bundle everything together, handle power etc etc. It
would make sense to just... buy a ready made device that can do that, and a coworker mentioned [Shelly](https://www.shelly.com/) to me. Apparently
their devices work with HA out of the box, enabling power control, metering and much more - most importantly, both in the form factor of a passthrough socket (like for my lamp) and small devices you can put behind a classical electric switch. I'll likely be ordering a couple of those for a test run.

