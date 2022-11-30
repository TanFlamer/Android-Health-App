package com.example.myapp.fragments.sport.sportStatistics;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databasefiles.type.Type;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class SportStatisticsAdapter extends RecyclerView.Adapter<SportStatisticsAdapter.SportRecyclerItemViewHolder> {

    Context context;
    List<Type> typeList;
    HashMap<Type, int[]> sportResults;
    HashMap<Type, Boolean> visibilityMap;

    public SportStatisticsAdapter(Context context, HashMap<Type, int[]> sportResults){
        this.context = context;
        this.typeList = new ArrayList<>(sportResults.keySet());
        this.sportResults = sportResults;

        visibilityMap = new HashMap<>();
        for(Type type : typeList) visibilityMap.put(type, false);
    }

    @NonNull
    @Override
    public SportRecyclerItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.sport_recycler_list_item, parent, false);
        return new SportRecyclerItemViewHolder(view);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull SportRecyclerItemViewHolder holder, int position) {
        Type type = typeList.get(position);
        int[] results = sportResults.get(type);
        holder.titleView.setText(type.getTypeName());
        holder.nameView.setText(type.getTypeName());
        holder.totalTimeView.setText(String.valueOf(results[0]));
        holder.totalCalorieView.setText(String.valueOf(results[0] * type.getCaloriePerMinute()));
        holder.totalDaysView.setText(String.valueOf(results[3]));
        holder.averageTimeView.setText(String.valueOf(results[0] / (double) results[3]));
        holder.averageCalorieView.setText(String.valueOf((results[0] * type.getCaloriePerMinute()) / (double) results[3]));
        holder.longestTimeView.setText(String.valueOf(results[1]));
        holder.shortestTimeView.setText(String.valueOf(results[2]));
        holder.mostCalorieView.setText(String.valueOf(results[1] * type.getCaloriePerMinute()));
        holder.leastCalorieView.setText(String.valueOf(results[2] * type.getCaloriePerMinute()));
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(type)) ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return typeList.size();
    }

    @SuppressLint("NotifyDataSetChanged")
    public void updateSportList(HashMap<Type, int[]> newSportResults, String data, String order){
        typeList.clear();
        typeList.addAll(newSportResults.keySet());
        sportResults.clear();
        sportResults.putAll(newSportResults);
        sortSportList(data, order);
    }

    @SuppressLint("NotifyDataSetChanged")
    public void sortSportList(String data, String order){
        typeList.sort(getComparator(data, order));
        visibilityMap.clear();
        for(Type type : typeList) visibilityMap.put(type, false);
        notifyDataSetChanged();
    }

    public Comparator<Type> getComparator(String data, String order){
        Comparator<Type> typeComparator = Comparator.comparingInt(Type::getTypeID);
        switch (data) {
            case "Date Added":
                typeComparator = Comparator.comparingInt(Type::getTypeID);
                break;
            case "Name":
                typeComparator = Comparator.comparing(Type::getTypeName);
                break;
            case "Total Days":
                typeComparator = Comparator.comparingInt(a -> getResults(a, 3));
                break;
            case "Total Duration":
                typeComparator = Comparator.comparingInt(a -> getResults(a, 0));
                break;
            case "Total Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 0) * a.getCaloriePerMinute());
                break;
            case "Average Duration":
                typeComparator = Comparator.comparingDouble(a -> (float) getResults(a, 0) / getResults(a, 3));
                break;
            case "Average Calorie":
                typeComparator = Comparator.comparingDouble(a -> (getResults(a, 0) * a.getCaloriePerMinute()) / getResults(a, 3));
                break;
            case "Max Duration":
                typeComparator = Comparator.comparingInt(a -> getResults(a, 1));
                break;
            case "Max Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 1) * a.getCaloriePerMinute());
                break;
            case "Min Duration":
                typeComparator = Comparator.comparingInt(a -> getResults(a, 2));
                break;
            case "Min Calorie":
                typeComparator = Comparator.comparingDouble(a -> getResults(a, 2) * a.getCaloriePerMinute());
                break;
        }
        return order.equals("Ascending") ? typeComparator : typeComparator.reversed();
    }

    public int getResults(Type type, int result){
        return Objects.requireNonNull(sportResults.get(type))[result];
    }

    public class SportRecyclerItemViewHolder extends RecyclerView.ViewHolder{

        TextView titleView, nameView, totalTimeView, totalCalorieView, averageTimeView, totalDaysView, averageCalorieView, longestTimeView, shortestTimeView, mostCalorieView, leastCalorieView;
        LinearLayout layoutVisible, layoutHidden;

        public SportRecyclerItemViewHolder(@NonNull View itemView) {
            super(itemView);

            titleView = itemView.findViewById(R.id.sportTitle);
            nameView = itemView.findViewById(R.id.sportName);
            totalTimeView = itemView.findViewById(R.id.sportTotalTime);
            totalCalorieView = itemView.findViewById(R.id.sportTotalCalorie);
            totalDaysView = itemView.findViewById(R.id.sportTotalDays);
            averageTimeView = itemView.findViewById(R.id.sportAverageTime);
            averageCalorieView = itemView.findViewById(R.id.sportAverageCalorie);
            longestTimeView = itemView.findViewById(R.id.sportLongestTime);
            shortestTimeView = itemView.findViewById(R.id.sportShortestTime);
            mostCalorieView = itemView.findViewById(R.id.sportMostCalorie);
            leastCalorieView = itemView.findViewById(R.id.sportLeastCalorie);

            layoutVisible = itemView.findViewById(R.id.sportLayoutVisible);
            layoutHidden = itemView.findViewById(R.id.sportLayoutHidden);

            layoutVisible.setOnClickListener(view -> {
                Type type = typeList.get(getAdapterPosition());
                visibilityMap.put(type, Boolean.FALSE.equals(visibilityMap.get(type)));
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
