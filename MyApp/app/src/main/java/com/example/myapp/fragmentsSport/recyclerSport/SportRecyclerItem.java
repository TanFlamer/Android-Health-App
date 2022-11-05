package com.example.myapp.fragmentsSport.recyclerSport;

public class SportRecyclerItem {

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getTotalTime() {
        return totalTime;
    }

    public void setTotalTime(int totalTime) {
        this.totalTime = totalTime;
    }

    public int getTotalCalorie() {
        return totalCalorie;
    }

    public void setTotalCalorie(int totalCalorie) {
        this.totalCalorie = totalCalorie;
    }

    public int getTotalDays() {
        return totalDays;
    }

    public void setTotalDays(int totalDays) {
        this.totalDays = totalDays;
    }

    public int getAverageTime() {
        return averageTime;
    }

    public void setAverageTime(int averageTime) {
        this.averageTime = averageTime;
    }

    public int getAverageCalorie() {
        return averageCalorie;
    }

    public void setAverageCalorie(int averageCalorie) {
        this.averageCalorie = averageCalorie;
    }

    public int getLongestTime() {
        return longestTime;
    }

    public void setLongestTime(int longestTime) {
        this.longestTime = longestTime;
    }

    public int getShortestTime() {
        return shortestTime;
    }

    public void setShortestTime(int shortestTime) {
        this.shortestTime = shortestTime;
    }

    public int getMostCalorie() {
        return mostCalorie;
    }

    public void setMostCalorie(int mostCalorie) {
        this.mostCalorie = mostCalorie;
    }

    public int getLeastCalorie() {
        return leastCalorie;
    }

    public void setLeastCalorie(int leastCalorie) {
        this.leastCalorie = leastCalorie;
    }

    public boolean isShown() {
        return shown;
    }

    public void setShown(boolean shown) {
        this.shown = shown;
    }

    String title;
    String name;
    int totalTime;
    int totalCalorie;
    int totalDays;
    int averageTime;
    int averageCalorie;
    int longestTime;
    int shortestTime;
    int mostCalorie;
    int leastCalorie;
    boolean shown;

    public SportRecyclerItem(String name, int totalTime, int totalCalorie, int totalDays, int longestTime, int shortestTime, int mostCalorie, int leastCalorie){
        this.title = name;
        this.name = name;
        this.totalTime = totalTime;
        this.totalCalorie = totalCalorie;
        this.totalDays = totalDays;
        this.averageTime = totalTime / totalDays;
        this.averageCalorie = totalCalorie / totalDays;
        this.longestTime = longestTime;
        this.shortestTime = shortestTime;
        this.mostCalorie = mostCalorie;
        this.leastCalorie = leastCalorie;
        this.shown = false;
    }
}
