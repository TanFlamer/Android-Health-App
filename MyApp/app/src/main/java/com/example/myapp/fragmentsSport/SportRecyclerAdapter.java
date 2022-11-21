package com.example.myapp.fragmentsSport;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Sport;

import java.util.HashMap;
import java.util.List;

public class SportRecyclerAdapter extends RecyclerView.Adapter<SportRecyclerAdapter.SportRecyclerItemViewHolder> {

    Context context;
    List<Sport> sportList;
    HashMap<Sport, Boolean> visibilityMap;

    public SportRecyclerAdapter(Context context, List<Sport> sportList){
        this.context = context;
        this.sportList = sportList;
        visibilityMap = new HashMap<>();
        for(Sport sport : sportList) visibilityMap.put(sport, false);
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
        Sport sport = sportList.get(position);
        holder.titleView.setText("sport.getTitle()");
        holder.nameView.setText("sport.getName()");
        holder.totalTimeView.setText("String.valueOf(sport.getTotalTime())");
        holder.totalCalorieView.setText("String.valueOf(sport.getTotalCalorie())");
        holder.totalDaysView.setText("String.valueOf(sport.getTotalDays())");
        holder.averageTimeView.setText("String.valueOf(sport.getAverageTime())");
        holder.averageCalorieView.setText("String.valueOf(sport.getAverageCalorie())");
        holder.longestTimeView.setText("String.valueOf(sport.getLongestTime())");
        holder.shortestTimeView.setText("String.valueOf(sport.getShortestTime())");
        holder.mostCalorieView.setText("String.valueOf(sport.getMostCalorie())");
        holder.leastCalorieView.setText("String.valueOf(sport.getLeastCalorie())");
        holder.layoutHidden.setVisibility(Boolean.TRUE.equals(visibilityMap.get(sport)) ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getItemCount() {
        return sportList.size();
    }

    public void updateSportList(List<Sport> newSportList){
        final SportRecyclerDiffCallback diffCallback = new SportRecyclerDiffCallback(sportList, newSportList);
        final DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(diffCallback);
        sportList.clear();
        sportList.addAll(newSportList);
        visibilityMap.clear();
        for(Sport sport : sportList) visibilityMap.put(sport, false);
        diffResult.dispatchUpdatesTo(this);
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
                Sport sport = sportList.get(getAdapterPosition());
                visibilityMap.put(sport, Boolean.FALSE.equals(visibilityMap.get(sport)));
                notifyItemChanged(getAdapterPosition());
            });
        }
    }
}
