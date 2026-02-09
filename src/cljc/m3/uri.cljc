;; Copyright 2025 Julian Gosnell
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns m3.uri
  (:require
   [clojure.string :refer [join split starts-with?]]
   [m3.util :refer [assoc-when]]))

;; not sure I like how paths and fragments fall out on the url side, but ... life is too short...
;; I don't think we are dealing with the possibility of a :type=:nss...
(defn parse-uri-2 [s]
  (if-let [[_ scheme nid nss _query fragment]
           (re-matches #"(?i)^(urn):([^:]+):([^?#]*)(?:\?([^#]*))?(?:#(.*))?" s)]
    ;; regexp insists on schema and nid
    (let [origin (str scheme ":" nid)
          nss (when-not (empty? nss) nss)]
      (-> {}
          (assoc      :type      (or (and origin :urn) (and nss :nss) :fragment))
          (assoc-when :origin    origin)
          (assoc-when :nss       nss)
          (assoc-when :fragment  fragment)))
    (if-let [[_ scheme authority path _query fragment]
             (re-matches #"(?i)^(?:(?:([a-z][a-z0-9+.-]*):)?(?://([^/?#]*))?([^?#]*))(?:\?([^#]*))?(?:#(.*))?" s)]
      ;; regexp does not insist on both schema and authority but... we'll pretend it does
      (let [origin (and scheme authority (str scheme "://" authority))
            path (when-not (empty? path) path)]
        (-> {}
            (assoc      :type     (or (and origin :url) (and path :path) :fragment))
            (assoc-when :origin   origin)
            (assoc-when :path     path)
            (assoc-when :fragment fragment)))
      ;; should never happen as id uris should have been validated against meta-schema
      (throw (ex-info "parse-uri failed:" {:uri s})))))

(def parse-uri (memoize parse-uri-2))

(defn strip-path [p]
  (let [s (join "/" (drop-last (split p #"/" -1)))]
    (when-not (empty? s) s)))

(defn- normalize-path
  "Remove . and .. segments from a URI path per RFC 3986 Section 5.2.4."
  [path]
  (let [segments (split path #"/" -1)
        ;; Preserve leading empty segment (absolute path starts with /)
        result (reduce
                (fn [acc seg]
                  (case seg
                    "." acc
                    ".." (if (and (seq acc) (not= (peek acc) ""))
                           (pop acc)
                           acc)
                    (conj acc seg)))
                []
                segments)]
    (join "/" result)))

(defn inherit-path [parent child]
  (if (starts-with? child "/")
    (normalize-path child)
    (normalize-path (str (and parent (strip-path parent)) "/" child))))

(defn inherit-uri [parent child]
  (if (nil? parent)
    child
    (let [{pt :type} parent
          {ct :type} child]
      (case [pt ct]
        [:urn      :urn]      child
        [:urn      :url]      child
        [:urn      :path]     child
        [:urn      :fragment] (assoc-when parent :fragment (:fragment child))
        [:url      :urn]      child
        [:url      :url]      child
        [:url      :path]     (let [{cp :path cf :fragment} child] (assoc-when (assoc parent :path (inherit-path (:path parent) cp)) :fragment cf))
        [:url      :fragment] (assoc-when parent :fragment (:fragment child))
        [:path     :urn]      child
        [:path     :url]      child
        [:path     :path]     (let [{cp :path cf :fragment} child] (assoc-when (assoc parent :path (inherit-path (:path parent) cp)) :fragment cf))
        [:path     :fragment] (assoc-when parent :fragment (:fragment child))
        [:fragment :urn]      child
        [:fragment :url]      child
        [:fragment :path]     child
        [:fragment :fragment] child
        (throw (ex-info "inherit-uri: unexpected type combination" {:parent pt :child ct}))))))

(defn uri-base [uri]
  (dissoc uri :fragment))

(defn uri-fragment [{t :type :as uri}]
  (if (= :fragment t)
    uri
    (assoc-when {:type :fragment} :fragment (:fragment uri))))
